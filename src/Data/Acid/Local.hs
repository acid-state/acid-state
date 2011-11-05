{-# LANGUAGE GADTs, OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving, BangPatterns, CPP #-}
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
    ( openLocalState
    , openLocalStateFrom
    , createArchive
    , createCheckpointAndClose
    ) where 

import Data.Acid.Log as Log
import Data.Acid.Core
import Data.Acid.Common
import qualified Data.Acid.Abstract as Abs

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
--import Control.Exception              ( evaluate )
import Control.Monad.State            ( runState )
import Control.Applicative            ( (<$>), (<*>) )
import Data.ByteString.Lazy           ( ByteString )
--import qualified Data.ByteString.Lazy as Lazy ( length )


import Data.Serialize                 ( runPutLazy, runGetLazy )
import Data.SafeCopy                  ( SafeCopy(..), safeGet, safePut
                                      , primitive, contain )
import Data.Typeable                  ( Typeable, typeOf, typeOf1 )
import System.FilePath                ( (</>) )


{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]  State changes are all-or-nothing. This is what you'd expect of any state
                   variable in Haskell and AcidState doesn't change that.

    [@Consistency@] No event or set of events will break your data invariants.

    [@Isolation@] Transactions cannot interfere with each other even when issued in parallel.

    [@Durability@] Successful transaction are guaranteed to survive system failure (both
                   hardware and software).
-}
data LocalState st
    = LocalState { localCore        :: Core st
                 , localEvents      :: FileLog (Tagged ByteString)
                 , localCheckpoints :: FileLog Checkpoint
                 } deriving (Typeable)

{-
-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => LocalState (EventState event) -> event -> IO (EventResult event)
update acidState event
    = takeMVar =<< scheduleUpdate acidState event
-}

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
scheduleUpdate :: UpdateEvent event => LocalState (EventState event) -> event -> IO (MVar (EventResult event))
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
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

scheduleColdUpdate :: LocalState st -> Tagged ByteString -> IO (MVar ByteString)
scheduleColdUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState coldMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) event $ putMVar mvar result
              return st'
         return mvar
    where coldMethod = lookupColdMethod (localCore acidState) event
{-
-- | Same as 'update' but lifted into any monad capable of doing IO.
update' :: (UpdateEvent event, MonadIO m) => LocalState (EventState event) -> event -> m (EventResult event)
update' acidState event
    = liftIO (update acidState event)
-}
-- | Issue a Query event and wait for its result. Events may be issued in parallel.
query  :: QueryEvent event  => LocalState (EventState event) -> event -> IO (EventResult event)
query acidState event
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do let (result, _st) = runState hotMethod st
              -- Make sure that we do not return the result before the event log has
              -- been flushed to disk.
              pushAction (localEvents acidState) $
                putMVar mvar result
         takeMVar mvar
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

-- Whoa, a buttload of refactoring is needed here. 2011-11-02
queryCold  :: LocalState st -> Tagged ByteString -> IO ByteString
queryCold acidState event
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do let (result, _st) = runState coldMethod st
              -- Make sure that we do not return the result before the event log has
              -- been flushed to disk.
              pushAction (localEvents acidState) $
                putMVar mvar result
         takeMVar mvar
    where coldMethod = lookupColdMethod (localCore acidState) event
{-
-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => LocalState (EventState event) -> event -> m (EventResult event)
query' acidState event
    = liftIO (query acidState event)
-}
-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--
--   This call will not return until the operation has succeeded.
createCheckpoint :: SafeCopy st => LocalState st -> IO ()
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
createCheckpointAndClose :: SafeCopy st => Abs.AcidState st -> IO ()
createCheckpointAndClose abstract_state
    = do mvar <- newEmptyMVar
         closeCore' (localCore acidState) $ \st ->
           do eventId <- askCurrentEntryId (localEvents acidState)
              pushAction (localEvents acidState) $
                pushEntry (localCheckpoints acidState) (Checkpoint eventId (runPutLazy (safePut st))) (putMVar mvar ())
         takeMVar mvar
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)
  where acidState = Abs.downcast abstract_state


data Checkpoint = Checkpoint EntryId ByteString

instance SafeCopy Checkpoint where
    kind = primitive
    putCopy (Checkpoint eventEntryId content)
        = contain $
          do safePut eventEntryId
             safePut content
    getCopy = contain $ Checkpoint <$> safeGet <*> safeGet


-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
openLocalState :: (Typeable st, IsAcidic st)
              => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                             --   found.
              -> IO (Abs.AcidState st)
openLocalState initialState
    = openLocalStateFrom ("state" </> show (typeOf initialState)) initialState

-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   Running two AcidState's from the same directory is an error
--   but will not result in dataloss.
openLocalStateFrom :: (IsAcidic st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (Abs.AcidState st)
openLocalStateFrom directory initialState
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
         return $ toAcidState LocalState { localCore = core
                                         , localEvents = eventsLog
                                         , localCheckpoints = checkpointsLog
                                         }

checkpointRestoreError msg
    = error $ "Could not parse saved checkpoint due to the following error: " ++ msg

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeAcidState :: LocalState st -> IO ()
closeAcidState acidState
    = do closeCore (localCore acidState)
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)


-- | Move all log files that are no longer necessary for state restoration into the 'Archive'
--   folder in the state directory. This folder can then be backed up or thrown out as you see fit.
--   Reverting to a state before the last checkpoint will not be possible if the 'Archive' folder
--   has been thrown out.
-- 
--   This method is idempotent and does not block the normal operation of the AcidState.
createArchive :: Abs.AcidState st -> IO ()
createArchive abstract_state
  = do -- We need to look at the last checkpoint saved to disk. Since checkpoints can be written
       -- in parallel with this call, we can't guarantee that the checkpoint we get really is the
       -- last one but that's alright.
       currentCheckpointId <- cutFileLog (localCheckpoints state)
       -- 'currentCheckpointId' is the ID of the next checkpoint that will be written to disk.
       -- 'currentCheckpointId-1' must then be the ID of a checkpoint on disk (or -1, of course).
       let durableCheckpointId = currentCheckpointId-1
       checkpoints <- readEntriesFrom (localCheckpoints state) durableCheckpointId
       case checkpoints of
         []      -> return ()
         (Checkpoint entryId _content : _)
           -> do -- 'entryId' is the lowest entryId that didn't contribute to the checkpoint.
                 -- 'archiveFileLog' moves all files that are lower than this entryId to the archive.
                 archiveFileLog (localEvents state) entryId
                 -- In the same style as above, we archive all log files that came before the log file
                 -- which contains our checkpoint.
                 archiveFileLog (localCheckpoints state) durableCheckpointId
  where state = Abs.downcast abstract_state

toAcidState :: IsAcidic st => LocalState st -> Abs.AcidState st
toAcidState local
  = Abs.AcidState { Abs._scheduleUpdate = scheduleUpdate local 
                  , Abs.scheduleColdUpdate = scheduleColdUpdate local 
                  , Abs._query = query local
                  , Abs.queryCold = queryCold local
                  , Abs.createCheckpoint = createCheckpoint local
                  , Abs.closeAcidState = closeAcidState local
                  , Abs.unsafeTag = typeOf1 local
                  , Abs.unsafeSubType = Abs.castToSubType local
                  }

