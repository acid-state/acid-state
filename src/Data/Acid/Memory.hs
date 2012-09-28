{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
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
import Data.Acid.Abstract

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
import Control.Monad.State            ( runState )
import Data.ByteString.Lazy           ( ByteString )
import Data.Typeable                  ( Typeable )
import Data.IORef                     ( IORef, newIORef, readIORef, writeIORef )

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
                  , localCopy    :: IORef st
                  } deriving (Typeable)

-- | Create an AcidState given an initial value.
openMemoryState :: (IsAcidic st)
              => st                          -- ^ Initial state value.
              -> IO (AcidState st)
openMemoryState initialState
    = do core <- mkCore (eventsToMethods acidEvents) initialState
         ref  <- newIORef initialState
         return $ toAcidState MemoryState { localCore = core, localCopy = ref }


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
scheduleMemoryUpdate :: UpdateEvent event => MemoryState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleMemoryUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              writeIORef (localCopy acidState) st'
              putMVar mvar result
              return st'
         return mvar
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

scheduleMemoryColdUpdate :: MemoryState st -> Tagged ByteString -> IO (MVar ByteString)
scheduleMemoryColdUpdate acidState event
    = do mvar <- newEmptyMVar
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState coldMethod st
              writeIORef (localCopy acidState) st'
              putMVar mvar result
              return st'
         return mvar
    where coldMethod = lookupColdMethod (localCore acidState) event

-- | Issue a Query event and wait for its result. Events may be issued in parallel.
memoryQuery  :: QueryEvent event  => MemoryState (EventState event) -> event -> IO (EventResult event)
memoryQuery acidState event
    = do st <- readIORef (localCopy acidState)
         let (result, _st) = runState hotMethod st
         return result
    where hotMethod = lookupHotMethod (coreMethods (localCore acidState)) event

memoryQueryCold  :: MemoryState st -> Tagged ByteString -> IO ByteString
memoryQueryCold acidState event
    = do st <- readIORef (localCopy acidState)
         let (result, _st) = runState coldMethod st
         return result
    where coldMethod = lookupColdMethod (localCore acidState) event

-- | This is a nop with the memory backend.
createMemoryCheckpoint :: SafeCopy st => MemoryState st -> IO ()
createMemoryCheckpoint acidState
    = return ()

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeMemoryState :: MemoryState st -> IO ()
closeMemoryState acidState
    = closeCore (localCore acidState)

toAcidState :: IsAcidic st => MemoryState st -> AcidState st
toAcidState memory
  = AcidState { _scheduleUpdate    = scheduleMemoryUpdate memory
              , scheduleColdUpdate = scheduleMemoryColdUpdate memory
              , _query             = memoryQuery memory
              , queryCold          = memoryQueryCold memory
              , createCheckpoint   = createMemoryCheckpoint memory
              , closeAcidState     = closeMemoryState memory
              , acidSubState       = mkAnyState memory
              }
