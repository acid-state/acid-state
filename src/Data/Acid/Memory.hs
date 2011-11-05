{-# LANGUAGE GADTs, OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving, BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Memory
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- AcidState container without a transaction log. Mostly used for testing. 
--

module Data.Acid.Memory
    ( openMemoryState
    ) where

import Data.Acid.Core
import Data.Acid.Common
import qualified Data.Acid.Abstract as Abs

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
import Control.Monad.State            ( runState )
import Control.Monad.Trans            ( MonadIO(liftIO) )
import Data.ByteString.Lazy           ( ByteString )
import Data.Typeable

import Data.SafeCopy                  ( SafeCopy(..) )


{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]  State changes are all-or-nothing. This is what you'd expect of any state
                   variable in Haskell and AcidState doesn't change that.

    [@Consistency@] No event or set of events will break your data invariants.

    [@Isolation@] Transactions cannot interfere with each other even when issued in parallel.

    [@Durability@] Successful transaction are guaranteed to survive system failure (both
                   hardware and software).
-}
data MemoryState st
    = MemoryState { localCore    :: Core st
                  } deriving (Typeable)
{-
-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => MemoryState (EventState event) -> event -> IO (EventResult event)
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
scheduleUpdate :: UpdateEvent event => MemoryState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              putMVar mvar result
              return st'
         return mvar
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

scheduleColdUpdate :: MemoryState st -> Tagged ByteString -> IO (MVar ByteString)
scheduleColdUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState coldMethod st
              putMVar mvar result
              return st'
         return mvar
    where coldMethod = lookupColdMethod (localCore acidState) event
{-
-- | Same as 'update' but lifted into any monad capable of doing IO.
update' :: (UpdateEvent event, MonadIO m) => MemoryState (EventState event) -> event -> m (EventResult event)
update' acidState event
    = liftIO (update acidState event)
-}
-- | Issue a Query event and wait for its result. Events may be issued in parallel.
query  :: QueryEvent event  => MemoryState (EventState event) -> event -> IO (EventResult event)
query acidState event
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do let (result, _st) = runState hotMethod st
              putMVar mvar result
         takeMVar mvar
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

queryCold  :: MemoryState st -> Tagged ByteString -> IO ByteString
queryCold acidState event
    = do mvar <- newEmptyMVar
         withCoreState (localCore acidState) $ \st ->
           do let (result, _st) = runState coldMethod st
              putMVar mvar result
         takeMVar mvar
    where coldMethod = lookupColdMethod (localCore acidState) event
{-
-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => MemoryState (EventState event) -> event -> m (EventResult event)
query' acidState event
    = liftIO (query acidState event)
-}

-- | This is a nop with the memory backend.
createCheckpoint :: SafeCopy st => MemoryState st -> IO ()
createCheckpoint acidState
    = return ()
{-
-- | This is an alias for 'closeAcidState' when using the memory backend.
createCheckpointAndClose :: SafeCopy st => MemoryState st -> IO ()
createCheckpointAndClose = closeAcidState
-}

-- | Create an AcidState given an initial value.
openMemoryState :: (IsAcidic st)
              => st                          -- ^ Initial state value. 
              -> IO (Abs.AcidState st)
openMemoryState initialState
    = do core <- mkCore (eventsToMethods acidEvents) initialState
         return $ toAcidState MemoryState { localCore = core }

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeAcidState :: MemoryState st -> IO ()
closeAcidState acidState
    = closeCore (localCore acidState)

toAcidState :: IsAcidic st => MemoryState st -> Abs.AcidState st
toAcidState memory
  = Abs.AcidState { Abs._scheduleUpdate = scheduleUpdate memory 
                  , Abs.scheduleColdUpdate = scheduleColdUpdate memory 
                  , Abs._query = query memory
                  , Abs.queryCold = queryCold memory
                  , Abs.createCheckpoint = createCheckpoint memory
                  , Abs.closeAcidState = closeAcidState memory
                  , Abs.unsafeTag = typeOf1 memory
                  , Abs.unsafeSubType = Abs.castToSubType memory
                  }
