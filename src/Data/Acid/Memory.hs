{-# LANGUAGE GADTs, OverloadedStrings, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving, BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Main
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- AcidState container without a transaction log. Mostly used for testing. 
--

module Data.Acid.Memory
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

import Data.Acid.Core
import Data.Acid.Common

import Control.Concurrent             ( newEmptyMVar, putMVar, takeMVar, MVar )
import Control.Monad.State            ( runState )
import Control.Monad.Trans            ( MonadIO(liftIO) )


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
data AcidState st
    = AcidState { localCore    :: Core st
                }

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
         modifyCoreState_ (localCore acidState) $ \st ->
           do let !(result, !st') = runState hotMethod st
              putMVar mvar result
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
              putMVar mvar result
         takeMVar mvar
    where hotMethod = lookupHotMethod (localCore acidState) event

-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
query' acidState event
    = liftIO (query acidState event)

-- | This is a nop with the memory backend.
createCheckpoint :: SafeCopy st => AcidState st -> IO ()
createCheckpoint acidState
    = return ()

-- | This is an alias for 'closeAcidState' when using the memory backend.
createCheckpointAndClose :: SafeCopy st => AcidState st -> IO ()
createCheckpointAndClose = closeAcidState


-- | Create an AcidState given an initial value.
openAcidState :: (IsAcidic st)
              => st                          -- ^ Initial state value. 
              -> IO (AcidState st)
openAcidState initialState
    = do core <- mkCore (eventsToMethods acidEvents) initialState
         return AcidState { localCore = core }

-- | Close an AcidState and associated logs.
--   Any subsequent usage of the AcidState will throw an exception.
closeAcidState :: AcidState st -> IO ()
closeAcidState acidState
    = closeCore (localCore acidState)

