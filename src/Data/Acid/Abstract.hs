{-# LANGUAGE RankNTypes, TypeFamilies, GADTs #-}
module Data.Acid.Abstract
    ( AcidState(..)
    , scheduleUpdate
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
import Control.Monad.Trans     ( MonadIO(liftIO) )
import Data.Typeable           ( Typeable1, gcast1, typeOf1 )

data AnyState st where
  AnyState :: Typeable1 sub_st => sub_st st -> AnyState st

-- Haddock doesn't get the types right on its own.
{-| State container offering full ACID (Atomicity, Consistency, Isolation and Durability)
    guarantees.

    [@Atomicity@]  State changes are all-or-nothing. This is what you'd expect of any state
                   variable in Haskell and AcidState doesn't change that.

    [@Consistency@] No event or set of events will break your data invariants.

    [@Isolation@] Transactions cannot interfere with each other even when issued in parallel.

    [@Durability@] Successful transaction are guaranteed to survive unexpected system shutdowns
                   (both those caused by hardware and software).
-}
data AcidState st
  = AcidState { 
                _scheduleUpdate :: forall event. (UpdateEvent event, EventState event ~ st) => event -> IO (MVar (EventResult event))
              , scheduleColdUpdate :: Tagged ByteString -> IO (MVar ByteString)
              , _query :: (QueryEvent event, EventState event ~ st)  => event -> IO (EventResult event)
              , queryCold :: Tagged ByteString -> IO ByteString
              , 
-- | Take a snapshot of the state and save it to disk. Creating checkpoints
--   makes it faster to resume AcidStates and you're free to create them as
--   often or seldom as fits your needs. Transactions can run concurrently
--   with this call.
--
--   This call will not return until the operation has succeeded.
                createCheckpoint :: IO ()
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
scheduleUpdate = _scheduleUpdate -- Redirection to make Haddock happy.

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
query = _query -- Redirection to make Haddock happy.

-- | Same as 'query' but lifted into any monad capable of doing IO.
query' :: (QueryEvent event, MonadIO m) => AcidState (EventState event) -> event -> m (EventResult event)
query' acidState event = liftIO (query acidState event)

mkAnyState :: Typeable1 sub_st => sub_st st -> AnyState st
mkAnyState = AnyState

downcast :: Typeable1 sub => AcidState st -> sub st
downcast AcidState{acidSubState = AnyState sub}
  = case gcast1 (Just sub) of
      Just (Just typed_sub_struct) -> typed_sub_struct `asTypeOf` result
      Nothing -> error $ "Data.Acid: Invalid subtype cast: " ++ show tag ++ " -> " ++ show (typeOf1 result)
  where result = undefined
        tag = show (typeOf1 sub)

