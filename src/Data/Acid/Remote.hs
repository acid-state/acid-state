module Data.Acid.Remote
    ( IsAcidic(..)
    , AcidState
    , Event(..)
    , EventResult
    , EventState
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , openRemote
    , closeAcidState
    , createCheckpoint
    , createCheckpointAndClose
    , update
    , scheduleUpdate
    , query
    , update'
    , query'
    , runQuery
      
      
    , acidServer
    ) where

import qualified Data.Acid.Local as Local
import Data.Acid.Core
import Data.Acid.Common

import Network
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Serialize
import Data.SafeCopy
import System.IO
import qualified Data.Sequence as Seq

acidServer :: SafeCopy st => Local.AcidState st -> PortID -> IO ()
acidServer acidState port
  = do socket <- listenOn port
       forever $ do (handle, _host, _port) <- accept socket
                    forkIO (process acidState handle)

data Command = RunQuery (Tagged Lazy.ByteString)
             | RunUpdate (Tagged Lazy.ByteString)
             | CreateCheckpoint

instance Serialize Command where
  put cmd = case cmd of
              RunQuery query   -> do putWord8 0; put query
              RunUpdate update -> do putWord8 1; put update
              CreateCheckpoint -> do putWord8 2
  get = do tag <- getWord8
           case tag of
             0 -> liftM RunQuery get
             1 -> liftM RunUpdate get
             2 -> return CreateCheckpoint

data Response = Result Lazy.ByteString | Acknowledgement

instance Serialize Response where
  put resp = case resp of
               Result result -> do putWord8 0; put result
               Acknowledgement -> putWord8 1
  get = do tag <- getWord8
           case tag of
             0 -> liftM Result get
             1 -> return Acknowledgement

process :: SafeCopy st => Local.AcidState st -> Handle -> IO ()
process acidState handle
  = do chan <- newChan
       forkIO $ forever $ do response <- join (readChan chan)
                             Strict.hPut handle (encode response)
                             hFlush handle
       worker chan (runGetPartial get Strict.empty)
  where worker chan inp
          = case inp of
              Fail msg      -> return () -- error msg
              Partial cont  -> do inp <- Strict.hGetSome handle 1024
                                  worker chan (cont inp)
              Done cmd rest -> do processCommand chan cmd; worker chan (runGetPartial get rest)
        processCommand chan cmd =
          case cmd of
            RunQuery query -> do result <- Local.queryCold acidState query
                                 writeChan chan (return $ Result result)
            RunUpdate update -> do result <- Local.scheduleColdUpdate acidState update
                                   writeChan chan (liftM Result $ takeMVar result)
            CreateCheckpoint -> do Local.createCheckpoint acidState
                                   writeChan chan (return Acknowledgement)


data AcidState st = AcidState (Command -> IO (MVar Response)) (IO ())

openRemote :: Local.IsAcidic st => HostName -> PortID -> IO (AcidState st)
openRemote host port
  = do handle <- connectTo host port
       writeLock <- newMVar ()
       -- callbacks are added to the right and read from the left
       callbacks <- newMVar (Seq.empty :: Seq.Seq (Response -> IO ()))
       isClosed <- newIORef False
       let getCallback =
               modifyMVar callbacks $ \s -> return $
               case Seq.viewl s of
                 Seq.EmptyL -> noCallback
                 (cb Seq.:< s') -> (s', cb)
           noCallback = error "openRemote: Internal error: Missing callback."
           newCallback cb = modifyMVar_ callbacks (\s -> return (s Seq.|> cb))
           
           listener inp
             = case inp of
                 Fail msg       -> error msg
                 Partial cont   -> do inp <- Strict.hGetSome handle 1024
                                      listener (cont inp)
                 Done resp rest -> do callback <- getCallback
                                      callback (resp :: Response)
                                      listener (runGetPartial get rest)
           actor cmd = do readIORef isClosed >>= closedError
                          ref <- newEmptyMVar
                          withMVar writeLock $ \() -> do
                            newCallback (putMVar ref)
                            Strict.hPut handle (encode cmd) >> hFlush handle
                          return ref

           closedError False = return ()
           closedError True  = throwIO $ ErrorCall "The AcidState has been closed"

           shutdown = do writeIORef isClosed True
                         hClose handle
       forkIO (listener (runGetPartial get Strict.empty))
       return (AcidState actor shutdown)

query :: QueryEvent event => AcidState (EventState event) -> event -> IO (EventResult event)
query (AcidState fn _shutdown) event
  = do let encoded = runPutLazy (safePut event)
       Result resp <- takeMVar =<< fn (RunQuery (methodTag event, encoded))
       return (case runGetLazyFix safeGet resp of
                 Left msg -> error msg
                 Right result -> result)

query' :: (MonadIO m, QueryEvent event) => AcidState (EventState event) -> event -> m (EventResult event)
query' acidState event = liftIO (query acidState event)

update :: UpdateEvent event => AcidState (EventState event) -> event -> IO (EventResult event)
update acidState event
  = takeMVar =<< scheduleUpdate acidState event

scheduleUpdate :: UpdateEvent event => AcidState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleUpdate (AcidState fn _shutdown) event
  = do let encoded = runPutLazy (safePut event)
       parsed <- newEmptyMVar
       respRef <- fn (RunUpdate (methodTag event, encoded))
       forkIO $ do Result resp <- takeMVar respRef
                   putMVar parsed (case runGetLazyFix safeGet resp of
                                      Left msg -> error msg
                                      Right result -> result)
       return parsed

update' :: (MonadIO m, UpdateEvent event) => AcidState (EventState event) -> event -> m (EventResult event)
update' acidState event = liftIO (update acidState event)

closeAcidState :: AcidState st -> IO ()
closeAcidState (AcidState _fn shutdown) = shutdown

createCheckpoint :: AcidState st -> IO ()
createCheckpoint (AcidState fn _shutdown)
  = do Acknowledgement <- takeMVar =<< fn CreateCheckpoint
       return ()

createCheckpointAndClose :: AcidState st -> IO ()
createCheckpointAndClose = createCheckpoint


