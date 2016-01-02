{-# LANGUAGE DeriveDataTypeable, BangPatterns, CPP #-}
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
    , prepareLocalState
    , prepareLocalStateFrom
    , scheduleLocalUpdate'
    , scheduleLocalColdUpdate'
    , createCheckpointAndClose
    , LocalState(..)
    , Checkpoint(..)
    ) where

import Data.Acid.Log as Log
import Data.Acid.Core
import Data.Acid.Common
import Data.Acid.Abstract

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
import Control.Exception              ( onException, evaluate )
import Control.Monad.State            ( runState )
import Control.Monad                  ( join )
import Control.Applicative            ( (<$>), (<*>) )
import Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( length )


import Data.Serialize                 ( runPutLazy, runGetLazy )
import Data.SafeCopy                  ( SafeCopy(..), safeGet, safePut
                                      , primitive, contain )
import Data.Typeable                  ( Typeable, typeOf )
import Data.IORef
import System.FilePath                ( (</>) )

import FileIO                         ( obtainPrefixLock, releasePrefixLock, PrefixLock )


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
                 , localCopy        :: IORef st
                 , localEvents      :: FileLog (Tagged ByteString)
                 , localCheckpoints :: FileLog Checkpoint
                 , localLock        :: PrefixLock
                 } deriving (Typeable)


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
scheduleLocalUpdate :: UpdateEvent event => LocalState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleLocalUpdate acidState event
    = do mvar <- newEmptyMVar
         let encoded = runPutLazy (safePut event)

         -- It is important that we encode the event now so that we can catch
         -- any exceptions (see nestedStateError in examples/errors/Exceptions.hs)
         evaluate (Lazy.length encoded)

         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) (methodTag event, encoded) $ do writeIORef (localCopy acidState) st'
                                                                                putMVar mvar result
              return st'
         return mvar
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

-- | Same as scheduleLocalUpdate but does not immediately change the localCopy
-- and return the result mvar - returns an IO action to do this instead. Take
-- care to run actions of multiple Updates in the correct order as otherwise
-- Queries will operate on outdated state.
scheduleLocalUpdate' :: UpdateEvent event => LocalState (EventState event) -> event -> MVar (EventResult event) -> IO (IO ())
scheduleLocalUpdate' acidState event mvar
    = do
         let encoded = runPutLazy (safePut event)

         -- It is important that we encode the event now so that we can catch
         -- any exceptions (see nestedStateError in examples/errors/Exceptions.hs)
         evaluate (Lazy.length encoded)

         act <- modifyCoreState (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) (methodTag event, encoded) $ return ()
              let action = do writeIORef (localCopy acidState) st'
                              putMVar mvar result
              return (st', action)
         -- this is the action to update state for queries and release the
         -- result into the supplied mvar
         return act
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

scheduleLocalColdUpdate :: LocalState st -> Tagged ByteString -> IO (MVar ByteString)
scheduleLocalColdUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState coldMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) event $ do writeIORef (localCopy acidState) st'
                                                           putMVar mvar result
              return st'
         return mvar
    where coldMethod = lookupColdMethod (localCore acidState) event

-- | Same as scheduleLocalColdUpdate but does not immediately change the
-- localCopy and return the result mvar - returns an IO action to do this
-- instead. Take care to run actions of multiple Updates in the correct order as
-- otherwise Queries will operate on outdated state.
scheduleLocalColdUpdate' :: LocalState st -> Tagged ByteString -> MVar ByteString -> IO (IO ())
scheduleLocalColdUpdate' acidState event mvar
    = do act <- modifyCoreState (localCore acidState) $ \st ->
           do let !(result, !st') = runState coldMethod st
              -- Schedule the log entry. Very important that it happens when 'localCore' is locked
              -- to ensure that events are logged in the same order that they are executed.
              pushEntry (localEvents acidState) event $ return ()
              let action = do writeIORef (localCopy acidState) st'
                              putMVar mvar result
              return (st', action)
         return act
    where coldMethod = lookupColdMethod (localCore acidState) event

-- | Issue a Query event and wait for its result. Events may be issued in parallel.
localQuery  :: QueryEvent event  => LocalState (EventState event) -> event -> IO (EventResult event)
localQuery acidState event
    = do st <- readIORef (localCopy acidState)
         let (result, _st) = runState hotMethod st
         return result
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

-- Whoa, a buttload of refactoring is needed here. 2011-11-02
localQueryCold  :: LocalState st -> Tagged ByteString -> IO ByteString
localQueryCold acidState event
    = do st <- readIORef (localCopy acidState)
         let (result, _st) = runState coldMethod st
         return result
    where coldMethod = lookupColdMethod (localCore acidState) event

-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--
--   This call will not return until the operation has succeeded.
createLocalCheckpoint :: SafeCopy st => LocalState st -> IO ()
createLocalCheckpoint acidState
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
createCheckpointAndClose :: (SafeCopy st, Typeable st) => AcidState st -> IO ()
createCheckpointAndClose abstract_state
    = do mvar <- newEmptyMVar
         closeCore' (localCore acidState) $ \st ->
           do eventId <- askCurrentEntryId (localEvents acidState)
              pushAction (localEvents acidState) $
                pushEntry (localCheckpoints acidState) (Checkpoint eventId (runPutLazy (safePut st))) (putMVar mvar ())
         takeMVar mvar
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)
         releasePrefixLock (localLock acidState)
  where acidState = downcast abstract_state


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
              -> IO (AcidState st)
openLocalState initialState =
  openLocalStateFrom ("state" </> show (typeOf initialState)) initialState

-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalState :: (Typeable st, IsAcidic st)
                  => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                                 --   found.
                  -> IO (IO (AcidState st))
prepareLocalState initialState =
  prepareLocalStateFrom ("state" </> show (typeOf initialState)) initialState


-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   Running two AcidState's from the same directory is an error
--   but will not result in dataloss.
openLocalStateFrom :: (IsAcidic st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (AcidState st)
openLocalStateFrom directory initialState =
  join $ resumeLocalStateFrom directory initialState False

-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in @directory@.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalStateFrom :: (IsAcidic st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (IO (AcidState st))
prepareLocalStateFrom directory initialState =
  resumeLocalStateFrom directory initialState True



resumeLocalStateFrom :: (IsAcidic st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> Bool                -- ^ True => load checkpoint before acquiring the lock.
                  -> IO (IO (AcidState st))
resumeLocalStateFrom directory initialState delayLocking =
  case delayLocking of
    True -> do
      (n, st) <- loadCheckpoint
      return $ do
        lock  <- obtainPrefixLock lockFile
        replayEvents lock n st
    False -> do
      lock    <- obtainPrefixLock lockFile
      (n, st) <- loadCheckpoint `onException` releasePrefixLock lock
      return $ do
        replayEvents lock n st
  where
    lockFile = directory </> "open"
    eventsLogKey = LogKey { logDirectory = directory
                          , logPrefix = "events" }
    checkpointsLogKey = LogKey { logDirectory = directory
                               , logPrefix = "checkpoints" }
    loadCheckpoint = do
      mbLastCheckpoint <- Log.newestEntry checkpointsLogKey
      case mbLastCheckpoint of
        Nothing ->
          return (0, initialState)
        Just (Checkpoint eventCutOff content) -> do
          case runGetLazy safeGet content of
            Left msg  -> checkpointRestoreError msg
            Right val -> return (eventCutOff, val)
    replayEvents lock n st = do
      core <- mkCore (eventsToMethods acidEvents) st

      eventsLog <- openFileLog eventsLogKey
      events <- readEntriesFrom eventsLog n
      mapM_ (runColdMethod core) events
      ensureLeastEntryId eventsLog n
      checkpointsLog <- openFileLog checkpointsLogKey
      stateCopy <- newIORef undefined
      withCoreState core (writeIORef stateCopy)

      return $ toAcidState LocalState { localCore = core
                                      , localCopy = stateCopy
                                      , localEvents = eventsLog
                                      , localCheckpoints = checkpointsLog
                                      , localLock = lock
                                      }

checkpointRestoreError msg
    = error $ "Could not parse saved checkpoint due to the following error: " ++ msg

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeLocalState :: LocalState st -> IO ()
closeLocalState acidState
    = do closeCore (localCore acidState)
         closeFileLog (localEvents acidState)
         closeFileLog (localCheckpoints acidState)
         releasePrefixLock (localLock acidState)

createLocalArchive :: SafeCopy st => LocalState st -> IO ()
createLocalArchive state
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
       -- If a checkpoint is created just before 'createArchive' is called, the
       -- last checkpoint file is empty, which corrupts the restoration of the
       -- state when the 'openLocalState' functions are thereafter called.
       -- To avoid this, we call 'createLocalCheckpoint' in order to push a new
       -- log entry.
       createLocalCheckpoint state

toAcidState :: IsAcidic st => LocalState st -> AcidState st
toAcidState local
  = AcidState { _scheduleUpdate = scheduleLocalUpdate local
              , scheduleColdUpdate = scheduleLocalColdUpdate local
              , _query = localQuery local
              , queryCold = localQueryCold local
              , createCheckpoint = createLocalCheckpoint local
              , createArchive = createLocalArchive local
              , closeAcidState = closeLocalState local
              , acidSubState = mkAnyState local
              }

