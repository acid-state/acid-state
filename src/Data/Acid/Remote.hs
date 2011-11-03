{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid.Remote
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 Network backend.

-}
module Data.Acid.Remote
    ( acidServer
    , openRemoteState
    ) where


import qualified Data.Acid.Abstract as Abs
import Data.Acid.Abstract (AcidState)
import Data.Acid.Core
import Data.Acid.Common

import Network
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Control.Exception (throwIO, ErrorCall(..), finally)
import Control.Monad
import Control.Concurrent
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Serialize
import Data.SafeCopy
import System.IO
import qualified Data.Sequence as Seq

{- | Accept connections on @port@ and serve requests using the given 'AcidState'.
     This call doesn't return.
 -}
acidServer :: SafeCopy st => Abs.AcidState st -> PortID -> IO ()
acidServer acidState port
  = do socket <- listenOn port
       forever $ do (handle, _host, _port) <- accept socket
                    forkIO (process acidState handle)
         `finally` sClose socket

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

process :: SafeCopy st => AcidState st -> Handle -> IO ()
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
            RunQuery query -> do result <- Abs.queryCold acidState query
                                 writeChan chan (return $ Result result)
            RunUpdate update -> do result <- Abs.scheduleColdUpdate acidState update
                                   writeChan chan (liftM Result $ takeMVar result)
            CreateCheckpoint -> do Abs.createCheckpoint acidState
                                   writeChan chan (return Acknowledgement)


data RemoteState st = RemoteState (Command -> IO (MVar Response)) (IO ())

{- | Connect to a remotely running 'AcidState'. -}
openRemoteState :: IsAcidic st => HostName -> PortID -> IO (Abs.AcidState st)
openRemoteState host port
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
       return (toAcidState $ RemoteState actor shutdown)

query :: QueryEvent event => RemoteState (EventState event) -> event -> IO (EventResult event)
query acidState event
  = do let encoded = runPutLazy (safePut event)
       resp <- queryCold acidState (methodTag event, encoded)
       return (case runGetLazyFix safeGet resp of
                 Left msg -> error msg
                 Right result -> result)

queryCold :: RemoteState st -> Tagged Lazy.ByteString -> IO Lazy.ByteString
queryCold (RemoteState fn _shutdown) event
  = do Result resp <- takeMVar =<< fn (RunQuery event)
       return resp

scheduleUpdate :: UpdateEvent event => RemoteState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleUpdate (RemoteState fn _shutdown) event
  = do let encoded = runPutLazy (safePut event)
       parsed <- newEmptyMVar
       respRef <- fn (RunUpdate (methodTag event, encoded))
       forkIO $ do Result resp <- takeMVar respRef
                   putMVar parsed (case runGetLazyFix safeGet resp of
                                      Left msg -> error msg
                                      Right result -> result)
       return parsed

scheduleColdUpdate :: RemoteState st -> Tagged Lazy.ByteString -> IO (MVar Lazy.ByteString)
scheduleColdUpdate (RemoteState fn _shutdown) event
  = do parsed <- newEmptyMVar
       respRef <- fn (RunUpdate event)
       forkIO $ do Result resp <- takeMVar respRef
                   putMVar parsed resp
       return parsed

closeAcidState :: RemoteState st -> IO ()
closeAcidState (RemoteState _fn shutdown) = shutdown

createCheckpoint :: RemoteState st -> IO ()
createCheckpoint (RemoteState fn _shutdown)
  = do Acknowledgement <- takeMVar =<< fn CreateCheckpoint
       return ()

-- I'm not sure what I feel about 'createCheckpointAndClose'. 2011-11-3 Lemmih.
--createCheckpointAndClose :: RemoteState st -> IO ()
--createCheckpointAndClose = createCheckpoint


toAcidState :: IsAcidic st => RemoteState st -> AcidState st
toAcidState remote
  = Abs.AcidState { Abs._scheduleUpdate = scheduleUpdate remote 
                  , Abs.scheduleColdUpdate = scheduleColdUpdate remote 
                  , Abs._query = query remote
                  , Abs.queryCold = queryCold remote
                  , Abs.createCheckpoint = createCheckpoint remote
                  , Abs.closeAcidState = closeAcidState remote
                  , Abs.unsafeSubType = Abs.castToSubType remote
                  , Abs.unsafeTag = "Remote"
                  }
