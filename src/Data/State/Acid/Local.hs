{-# LANGUAGE GADTs, OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             MagicHash, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.State.Acid.Local
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  portable
--
-- AcidState container using a transaction log on disk. The term \'Event\' is
-- loosely used for transactions with ACID guarantees. \'Method\' is loosely
-- used for state operations without ACID guarantees (see "Data.State.Acid.Core").
--

module Data.State.Acid.Local
    ( AcidState
    , Event(..)
    , EventResult
    , UpdateEvent
    , QueryEvent
    , mkAcidState
    , closeAcidState
    , createCheckpoint
    , update
    , query
    ) where

import Data.State.Acid.Log as Log
import Data.State.Acid.Core

import Control.Concurrent
import qualified Control.Monad.State as State
import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString.Lazy as Lazy

import Data.Binary
import Data.Typeable
import System.FilePath

-- | Events return the same thing as Methods. The exact type of 'EventResult'
--   depends on the event.
type EventResult ev = MethodResult ev

-- | We distinguish between events that modify the state and those that do not.
--
--   UpdateEvents are executed in a MonadState context and have to be serialized
--   to disk before they are considered durable.
--
--   QueryEvents are executed in a MonadReader context and obviously do not have
--   to be serialized to disk.
data Event st where
    UpdateEvent :: UpdateEvent ev => (ev -> Update st (EventResult ev)) -> Event st
    QueryEvent  :: QueryEvent  ev => (ev -> Query st (EventResult ev)) -> Event st

-- | All UpdateEvents are also Methods.
class Method ev => UpdateEvent ev
-- | All QueryEvents are also Methods.
class Method ev => QueryEvent ev


eventsToMethods :: [Event st] -> [MethodContainer st]
eventsToMethods = map worker
    where worker (UpdateEvent fn) = Method (unUpdate . fn)
          worker (QueryEvent fn)  = Method (\ev -> do st <- State.get
                                                      return (runReader (unQuery $ fn ev) st)
                                           )
{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]  State changes are all-or-nothing. This is what you'd expect of any state
                   variable in Haskell and AcidState doesn't change that.

    [@Consistency@] No event or set of events will break your data invariants. This includes
                    power outages, 

    [@Isolation@] Transactions cannot interfere with each other even when issued in parallel.

    [@Durability@] Successful transaction are guaranteed to survive system failure (both
                   hardware and software).
-}
data AcidState st
    = AcidState { localCore        :: Core st
                , localEvents      :: FileLog (Tagged Lazy.ByteString)
                , localCheckpoints :: FileLog Checkpoint
                }

newtype Update st a = Update { unUpdate :: State.State st a }
    deriving (Monad, State.MonadState st)
newtype Query st a  = Query { unQuery :: Reader st a }
    deriving (Monad, MonadReader st)

-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--   
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => AcidState st -> event -> IO (EventResult event)
update acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let (result, st') = State.runState hotMethod st
              -- schedule the log entry. Very important that it happens when 'localCore' is locked.
              pushEntry (localEvents acidState) (methodTag event, encode event) $ putMVar mvar result
              return st'
         takeMVar mvar
    where hotMethod = lookupHotMethod (localCore acidState) event

-- | Issue a Query event and wait for its result. Events may be issued in parallel.
query  :: QueryEvent event  => AcidState st -> event -> IO (EventResult event)
query acidState event
    = runHotMethod (localCore acidState) event

-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--   
--   This call will not return until the operation has succeeded.
createCheckpoint :: Binary st => AcidState st -> IO ()
createCheckpoint acidState
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do eventId <- askCurrentEntryId (localEvents acidState)
              pushEntry (localCheckpoints acidState) (Checkpoint eventId (encode st)) (putMVar mvar ())
         takeMVar mvar
         


data Checkpoint = Checkpoint EntryId Lazy.ByteString

instance Binary Checkpoint where
    put (Checkpoint eventEntryId content)
        = do put eventEntryId
             put content
    get = Checkpoint <$> get <*> get


-- | Create an AcidState given a list of events (aka. transactions) and an initial value.
--   
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
mkAcidState :: (Typeable st, Binary st)
            => [Event st]                -- ^ List of events capable of updating or querying the state.
            -> st                        -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
            -> IO (AcidState st)
mkAcidState events initialState
    = do core <- mkCore (eventsToMethods events) initialState
         let directory = "state" </> show (typeOf initialState)
         let eventsLogKey = LogKey { logDirectory = directory
                                   , logPrefix = "events" }
             checkpointsLogKey = LogKey { logDirectory = directory
                                        , logPrefix = "checkpoints" }
         mbLastCheckpoint <- Log.newestEntry checkpointsLogKey
         n <- case mbLastCheckpoint of
                Nothing
                  -> return 0
                Just (Checkpoint eventCutOff content)
                  -> do modifyCoreState_ core (\_oldState -> return (decode content))
                        return eventCutOff
         events <- entriesAfterCutoff eventsLogKey n
         mapM_ (runColdMethod core) events
         eventsLog <- openFileLog eventsLogKey
         checkpointsLog <- openFileLog checkpointsLogKey
         return AcidState { localCore = core
                          , localEvents = eventsLog
                          , localCheckpoints = checkpointsLog
                          }

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeAcidState :: AcidState st -> IO ()
closeAcidState acidState
    = do closeCore (localCore acidState)
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)

