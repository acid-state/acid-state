{-# LANGUAGE GADTs, OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Local
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- AcidState container using a transaction log on disk. The term \'Event\' is
-- loosely used for transactions with ACID guarantees. \'Method\' is loosely
-- used for state operations without ACID guarantees (see "Data.Acid.Core").
--

module Data.Acid.Local
    ( IsAcidic(..)
    , AcidState
    , Event(..)
    , EventResult
    , EventState
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , openAcidState
    , openAcidStateFrom
    , closeAcidState
    , createCheckpoint
    , createCheckpointAndClose
    , update
    , scheduleUpdate
    , query
    , update'
    , query'
    , runQuery
    ) where

import Data.Acid.Log as Log
import Data.Acid.Core

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
--import Control.Exception              ( evaluate )
import Control.Monad.State            ( MonadState, State, get, runState )
import Control.Monad.Reader           ( Reader, runReader, MonadReader )
import Control.Monad.Trans            ( MonadIO(liftIO) )
import Control.Applicative            ( (<$>), (<*>) )
import Data.ByteString.Lazy           ( ByteString )
--import qualified Data.ByteString.Lazy as Lazy ( length )


import Data.Serialize                 ( runPutLazy, runGetLazy )
import Data.SafeCopy                  ( SafeCopy(..), safeGet, safePut
                                      , primitive, contain )
import Data.Typeable                  ( Typeable, typeOf )
import System.FilePath                ( (</>) )

-- | Events return the same thing as Methods. The exact type of 'EventResult'
--   depends on the event.
type EventResult ev = MethodResult ev

type EventState ev = MethodState ev

-- | We distinguish between events that modify the state and those that do not.
--
--   UpdateEvents are executed in a MonadState context and have to be serialized
--   to disk before they are considered durable.
--
--   QueryEvents are executed in a MonadReader context and obviously do not have
--   to be serialized to disk.
data Event st where
    UpdateEvent :: UpdateEvent ev => (ev -> Update (EventState ev) (EventResult ev)) -> Event (EventState ev)
    QueryEvent  :: QueryEvent  ev => (ev -> Query (EventState ev) (EventResult ev)) -> Event (EventState ev)

-- | All UpdateEvents are also Methods.
class Method ev => UpdateEvent ev
-- | All QueryEvents are also Methods.
class Method ev => QueryEvent ev


eventsToMethods :: [Event st] -> [MethodContainer st]
eventsToMethods = map worker
    where worker :: Event st -> MethodContainer st
          worker (UpdateEvent fn) = Method (unUpdate . fn)
          worker (QueryEvent fn)  = Method (\ev -> do st <- get
                                                      return (runReader (unQuery $ fn ev) st)
                                           )
{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]  State changes are all-or-nothing. This is what you'd expect of any state
                   variable in Haskell and AcidState doesn't change that.

    [@Consistency@] No event or set of events will break your data invariants.

    [@Isolation@] Transactions cannot interfere with each other even when issued in parallel.

    [@Durability@] Successful transaction are guaranteed to survive system failure (both
                   hardware and software).
-}
data AcidState st
    = AcidState { localCore        :: Core st
                , localEvents      :: FileLog (Tagged ByteString)
                , localCheckpoints :: FileLog Checkpoint
                }

-- | Context monad for Update events.
newtype Update st a = Update { unUpdate :: State st a }
    deriving (Monad, MonadState st)

-- | Context monad for Query events.
newtype Query st a  = Query { unQuery :: Reader st a }
    deriving (Monad, MonadReader st)

-- | Run a query in the Update Monad.
runQuery :: Query st a -> Update st a
runQuery query
    = do st <- get
         return (runReader (unQuery query) st)

-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--   
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => AcidState (EventState event) -> event -> IO (EventResult event)
update acidState event
    = takeMVar =<< scheduleUpdate acidState event

-- | Issue an Update event and return immediately. The event is not durable
--   before the MVar has been filled but the order of events is honored.
--   The behavior in case of exceptions is exactly the same as for 'update'.
--
--   If EventA is scheduled before EventB, EventA /will/ be executed before EventB:
--
--   @
--do scheduleUpdate acid EventA
--   scheduleUpdate acid EventB
--   @
scheduleUpdate :: UpdateEvent event => AcidState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleUpdate acidState event
    = do mvar <- newEmptyMVar
         let encoded = runPutLazy (safePut event)
         --evaluate (Lazy.length encoded) -- It would be best to encode the event before we lock the core
                                          -- but it hurts performance /-:
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) (methodTag event, encoded) $ putMVar mvar result
              return st'
         return mvar
    where hotMethod = lookupHotMethod (localCore acidState) event

-- | Same as 'update' but lifted into any monad capable of doing IO.
update' :: (UpdateEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
update' acidState event
    = liftIO (update acidState event)

-- | Issue a Query event and wait for its result. Events may be issued in parallel.
query  :: QueryEvent event  => AcidState (EventState event) -> event -> IO (EventResult event)
query acidState event
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do let (result, _st) = runState hotMethod st
              -- Make sure that we do not return the result before the event log has
              -- been flushed to disk.
              pushAction (localEvents acidState) $
                putMVar mvar result
         takeMVar mvar
    where hotMethod = lookupHotMethod (localCore acidState) event

-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
query' acidState event
    = liftIO (query acidState event)

-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--   
--   This call will not return until the operation has succeeded.
createCheckpoint :: SafeCopy st => AcidState st -> IO ()
createCheckpoint acidState
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do eventId <- askCurrentEntryId (localEvents acidState)
              pushAction (localEvents acidState) $
                do let encoded = runPutLazy (safePut st)
                   pushEntry (localCheckpoints acidState) (Checkpoint eventId encoded) (putMVar mvar ())
         takeMVar mvar

-- | Save a snapshot to disk and close the AcidState as a single atomic
--   action. This is useful when you want to make sure that no events
--   are saved to disk after a checkpoint.
createCheckpointAndClose :: SafeCopy st => AcidState st -> IO ()
createCheckpointAndClose acidState
    = do mvar <- newEmptyMVar
         closeCore' (localCore acidState) $ \st ->
           do eventId <- askCurrentEntryId (localEvents acidState)
              pushAction (localEvents acidState) $
                pushEntry (localCheckpoints acidState) (Checkpoint eventId (runPutLazy (safePut st))) (putMVar mvar ())
         takeMVar mvar
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)


data Checkpoint = Checkpoint EntryId ByteString

instance SafeCopy Checkpoint where
    kind = primitive
    putCopy (Checkpoint eventEntryId content)
        = contain $
          do safePut eventEntryId
             safePut content
    getCopy = contain $ Checkpoint <$> safeGet <*> safeGet

class (SafeCopy st) => IsAcidic st where
    acidEvents :: [Event st]
      -- ^ List of events capable of updating or querying the state.

-- | Create an AcidState given an initial value.
--   
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
openAcidState :: (Typeable st, IsAcidic st)
              => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                             --   found.
              -> IO (AcidState st)
openAcidState initialState
    = openAcidStateFrom ("state" </> show (typeOf initialState)) initialState

-- | Create an AcidState given a log directory and an initial value.
--   
--   This will create or resume a log found in @directory@.
--   Running two AcidState's from the same directory is an error
--   but will not result in dataloss.
openAcidStateFrom :: (IsAcidic st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (AcidState st)
openAcidStateFrom directory initialState
    = do core <- mkCore (eventsToMethods acidEvents) initialState
         let eventsLogKey = LogKey { logDirectory = directory
                                   , logPrefix = "events" }
             checkpointsLogKey = LogKey { logDirectory = directory
                                        , logPrefix = "checkpoints" }
         mbLastCheckpoint <- Log.newestEntry checkpointsLogKey
         n <- case mbLastCheckpoint of
                Nothing
                  -> return 0
                Just (Checkpoint eventCutOff content)
                  -> do modifyCoreState_ core (\_oldState -> case runGetLazy safeGet content of
                                                               Left msg  -> checkpointRestoreError msg
                                                               Right val -> return val)
                        return eventCutOff
         
         eventsLog <- openFileLog eventsLogKey
         events <- readEntriesFrom eventsLog n
         mapM_ (runColdMethod core) events
         checkpointsLog <- openFileLog checkpointsLogKey
         return AcidState { localCore = core
                          , localEvents = eventsLog
                          , localCheckpoints = checkpointsLog
                          }

checkpointRestoreError msg
    = error $ "Could not parse saved checkpoint due to the following error: " ++ msg

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeAcidState :: AcidState st -> IO ()
closeAcidState acidState
    = do closeCore (localCore acidState)
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)

