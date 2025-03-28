{-# LANGUAGE RankNTypes, TypeFamilies, GADTs, CPP, TypeOperators #-}
module Data.Acid.Abstract
    ( AcidState(..)
    , scheduleUpdate
    , groupUpdates
    , update
    , update'
    , query
    , query'
    , mkAnyState
    , downcast
    ) where

import Data.Acid.Common

import Data.Acid.Core

import Control.Concurrent      ( MVar, takeMVar ) 
import Data.ByteString.Lazy    ( ByteString )
import Control.Monad           ( void )
import Control.Monad.Trans     ( MonadIO(liftIO) )
#if __GLASGOW_HASKELL__ >= 707
import Data.Typeable           ( Typeable, gcast, typeOf )
#else
import Data.Typeable           ( Typeable1, gcast1, typeOf1 )
#endif

data AnyState st where
#if __GLASGOW_HASKELL__ >= 707
  AnyState :: Typeable sub_st => sub_st st -> AnyState st
#else
  AnyState :: Typeable1 sub_st => sub_st st -> AnyState st
#endif

-- Haddock doesn't get the types right on its own.
{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]   State changes are all-or-nothing. This is what you'd expect
                    of any state variable in Haskell and AcidState doesn't
                    change that.
    [@Consistency@] No event or set of events will break your data invariants.
    [@Isolation@]   Transactions cannot interfere with each other even when
                    issued in parallel.
    [@Durability@]  Successful transaction are guaranteed to survive unexpected
                    system shutdowns (both those caused by hardware and software).
-}
data AcidState st
  = AcidState {
                _scheduleUpdate :: forall event. (UpdateEvent event, EventState event ~ st) => event -> IO (MVar (EventResult event))
              , scheduleColdUpdate :: Tagged ByteString -> IO (MVar ByteString)
              , _query :: forall event. (QueryEvent event, EventState event ~ st) => event -> IO (EventResult event)
              , queryCold :: Tagged ByteString -> IO ByteString
              ,
-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--
--   This call will not return until the operation has succeeded.
                createCheckpoint :: IO ()
-- | Move all log files that are no longer necessary for state restoration into the 'Archive'
--   folder in the state directory. This folder can then be backed up or thrown out as you see fit.
--   Reverting to a state before the last checkpoint will not be possible if the 'Archive' folder
--   has been thrown out.
--
--   This method is idempotent and does not block the normal operation of the AcidState.
              , createArchive :: IO ()
              ,
-- | Close an AcidState and associated resources.
--   Any subsequent usage of the AcidState will throw an exception.
                closeAcidState :: IO ()
              , acidSubState :: AnyState st
              }

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
scheduleUpdate acid = _scheduleUpdate acid -- Redirection to make Haddock happy.

-- | Schedule multiple Update events and wait for them to be durable, but
--   throw away their results. This is useful for importing existing
--   datasets into an AcidState.
groupUpdates :: UpdateEvent event => AcidState (EventState event) -> [event] -> IO ()
groupUpdates acidState events
  = go events
  where
    go []     = return ()
    go [x]    = void $ update acidState x
    go (x:xs) = scheduleUpdate acidState x >> go xs

-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => AcidState (EventState event) -> event -> IO (EventResult event)
update acidState event = takeMVar =<< scheduleUpdate acidState event

-- | Same as 'update' but lifted into any monad capable of doing IO.
update' :: (UpdateEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
update' acidState event = liftIO (update acidState event)

-- | Issue a Query event and wait for its result. Events may be issued in parallel.
query :: QueryEvent event => AcidState (EventState event) -> event -> IO (EventResult event)
query acid = _query acid -- Redirection to make Haddock happy.

-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
query' acidState event = liftIO (query acidState event)

#if __GLASGOW_HASKELL__ >= 707
mkAnyState :: Typeable sub_st => sub_st st -> AnyState st
#else
mkAnyState :: Typeable1 sub_st => sub_st st -> AnyState st
#endif
mkAnyState = AnyState

#if __GLASGOW_HASKELL__ >= 707
downcast :: (Typeable sub, Typeable st) => AcidState st -> sub st
downcast AcidState{acidSubState = AnyState sub}
  = r
 where
   r = case gcast (Just sub) of
         Just (Just x) -> x
         _ ->
           error $
            "Data.Acid.Abstract: Invalid subtype cast: " ++ show (typeOf sub) ++ " -> " ++ show (typeOf r)
#else
downcast :: Typeable1 sub => AcidState st -> sub st
downcast AcidState{acidSubState = AnyState sub}
  = r
 where
   r = case gcast1 (Just sub) of
         Just (Just x) -> x
         _ ->
           error $
            "Data.Acid.Abstract: Invalid subtype cast: " ++ show (typeOf1 sub) ++ " -> " ++ show (typeOf1 r)
#endif
