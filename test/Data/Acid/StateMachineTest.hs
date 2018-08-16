{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides a general framework for state-machine testing of
-- acid-state components.  It needs to be instantiated with a
-- particular acid-state component to get a concrete test.
module Data.Acid.StateMachineTest
  ( acidUpdate
  , acidQuery
  , acidStateSequentialProperty
  , acidStateParallelProperty
  , restoreOldStateProperty
  , acidStateInterface
  , AcidStateInterface(..)

  , Model(..)
  , open
  , close
  , checkpoint
  , checkpointClose
  , kill
  ) where

import           Control.Exception (IOException, catch)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Acid as Acid
import qualified Data.Acid.Common as Common
import qualified Data.Acid.Core as Core
import qualified Data.Acid.Local as Local
import           Data.Maybe
import qualified Data.SafeCopy as SafeCopy
import           Data.Typeable
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           System.Directory (removeDirectoryRecursive, removeFile)


-- | Model of an acid-state component, for state machine property
-- testing.  We start in 'StateAbsent' and can transition to
-- 'StateOpen' by opening the state for the first time.  Thereafter we
-- can transition between 'StateOpen' and 'StateClosed' by opening and
-- closing the state.  Both of the latter keep track of the current
-- value of the state.
data Model s (v :: * -> *)
    = StateAbsent    -- ^ State is not present on disk
    | StateClosed s  -- ^ State is present on disk, but not in memory
    | StateOpen   s (Var (Opaque (Acid.AcidState s)) v) -- ^ State is open in memory

-- | Return the handle to the acid-state component if it is open, or
-- return 'Nothing' if it is closed or absent.
modelHandle :: Model s v -> Maybe (Var (Opaque (Acid.AcidState s)) v)
modelHandle (StateOpen _ hdl) = Just hdl
modelHandle _                 = Nothing

-- | Is the state currently open?
isOpen :: Model s v -> Bool
isOpen = isJust . modelHandle

-- | Return the current value of the state, if it is present.
modelValue :: Model s v -> Maybe s
modelValue (StateClosed s) = Just s
modelValue (StateOpen s _) = Just s
modelValue StateAbsent     = Nothing


-- | Description of the interface for performing
-- meta-operations on an acid-state, such as opening, closing and
-- checkpointing it.  The testing code is abstracted over this
-- interface so that we can substitute alternate serialisation
-- backends.
data AcidStateInterface s =
    (Typeable s, Acid.IsAcidic s, Show s)
    => AcidStateInterface
        { openState            :: s -> IO (Acid.AcidState s)
        , closeState           :: Acid.AcidState s -> IO ()
        , checkpointState      :: Acid.AcidState s -> IO ()
        , checkpointCloseState :: Acid.AcidState s -> IO ()
        , resetState           :: IO ()
        , statePath            :: FilePath
        }

-- | Standard implementation of the acid-state interface, using
-- 'SafeCopy'-based serialisation.
--
-- This takes the path to the state directory as an argument.
-- Warning: this path will be deleted/overwritten!
acidStateInterface :: (Acid.IsAcidic s, SafeCopy.SafeCopy s, Typeable s, Show s)
                   => FilePath -> AcidStateInterface s
acidStateInterface fp =
    AcidStateInterface { openState            = Acid.openLocalStateFrom fp
                       , closeState           = Acid.closeAcidState
                       , checkpointState      = Acid.createCheckpoint
                       , checkpointCloseState = Local.createCheckpointAndClose
                       , resetState           = removeDirectoryRecursive fp
                                                  `catch` (\ (_ :: IOException) -> return ())
                       , statePath            = fp
                       }


data Open s (v :: * -> *) = Open s
  deriving Show

instance HTraversable (Open s) where
  htraverse _ (Open s) = pure (Open s)

-- | Command to open the state, given the interface to use and a
-- generator for possible initial state values.
open :: MonadIO m => AcidStateInterface s -> Gen s -> Command Gen m (Model s)
open AcidStateInterface{..} gen_initial_state = Command gen execute [ Require require, Update update ]
  where
    require model _ = not (isOpen model)

    gen StateAbsent   = Just (Open <$> gen_initial_state)
    gen StateClosed{} = Just (Open <$> gen_initial_state)
    gen StateOpen{}   = Nothing

    execute (Open initial_state) = liftIO (Opaque <$> openState initial_state)

    update StateAbsent     (Open s) hdl = StateOpen s hdl
    update (StateClosed s) (Open _) hdl = StateOpen s hdl
    update StateOpen{}     _        _   = error "open: state already open!"


data WithState s (v :: * -> *) = WithState String (Var (Opaque (Acid.AcidState s)) v)
  deriving (Show)

instance HTraversable (WithState s) where
  htraverse k (WithState l v) = WithState l <$> htraverse k v

genWithState :: Applicative g => String -> Model s v -> Maybe (g (WithState s v))
genWithState l model = pure . WithState l <$> modelHandle model

-- | Command to close the state.
close :: MonadIO m => AcidStateInterface s -> Command Gen m (Model s)
close AcidStateInterface{..} = Command (genWithState "Close") execute [ Require require, Update update ]
  where
    require model _ = isOpen model

    execute (WithState _ (Var (Concrete (Opaque st)))) = liftIO (closeState st)
    update (StateOpen s _) _ _ = StateClosed s
    update _               _ _ = error "close: not open"

-- | Command to take a checkpoint of the state.
checkpoint :: MonadIO m => AcidStateInterface s -> Command Gen m (Model s)
checkpoint AcidStateInterface{..} = Command (genWithState "checkpoint") execute [ Require require ]
  where
    require model _ = isOpen model
    execute (WithState _ (Var (Concrete (Opaque st)))) = liftIO (checkpointState st)

-- | Command to take a checkpoint and close the state, as a single atomic action.
checkpointClose :: MonadIO m => AcidStateInterface s -> Command Gen m (Model s)
checkpointClose AcidStateInterface{..} =
    Command (genWithState "checkpointClose") execute [ Require require, Update update ]
  where
    require model _ = isOpen model
    execute (WithState _ (Var (Concrete (Opaque st)))) = liftIO (checkpointCloseState st)
    update (StateOpen s _) _ _ = StateClosed s
    update _               _ _ = error "checkpointClose: not open"

-- | Command to simulate killing the process without closing the
-- state.  This does not actually stop the old thread, though.
--
-- The lock file is removed so that the state can be reopened
-- (otherwise further commands would immediately fail).
kill :: MonadIO m => AcidStateInterface s -> Command Gen m (Model s)
kill AcidStateInterface{..} = Command (genWithState "kill") execute [ Require require, Update update ]
  where
    require model _ = isOpen model
    execute WithState{} = liftIO $ removeFile (statePath ++ "/open.lock")
    update (StateOpen s _) _ _ = StateClosed s
    update _               _ _ = error "kill: not open"



data AcidCommand s e (v :: * -> *) = AcidCommand e (Var (Opaque (Acid.AcidState s)) v)
  deriving (Show)

instance HTraversable (AcidCommand s e) where
  htraverse k (AcidCommand e s) = AcidCommand e <$> htraverse k s

lookupMethod :: (Core.Method m, Acid.IsAcidic (Core.MethodState m))
             => m -> State (Core.MethodState m) (Core.MethodResult m)
lookupMethod m = Core.lookupHotMethod mmap m
  where
    mmap = Core.mkMethodMap (Common.eventsToMethods Common.acidEvents)

-- | Translate an acid-state update into a command that executes the
-- update, given a generator of inputs.
acidUpdate :: forall s e m .
              ( Acid.IsAcidic s
              , Acid.EventState e ~ s
              , Acid.UpdateEvent e
              , Show e
              , Eq (Acid.EventResult e)
              , Show (Acid.EventResult e)
              , Typeable (Acid.EventResult e)
              , MonadIO m
              )
           => Gen e -> Command Gen m (Model s)
acidUpdate gen_event = Command gen execute [ Require require, Update update, Ensure ensure ]
  where
    -- Generate updates only when state is open
    gen :: Model s Symbolic -> Maybe (Gen (AcidCommand s e Symbolic))
    gen model = case modelHandle model of
                  Just st -> Just (AcidCommand <$> gen_event <*> pure st)
                  Nothing -> Nothing

    -- Execute a concrete update directly using acid-state
    execute :: AcidCommand s e Concrete -> m (Acid.EventResult e)
    execute (AcidCommand e (Var (Concrete (Opaque st)))) = liftIO (Acid.update st e)

    -- Shrinking updates requires the state to be open
    require model _ = isOpen model

    -- Updates cause the model state to be updated
    update (StateOpen s hdl) (AcidCommand c _) _ = StateOpen (execState (lookupMethod c) s) hdl
    update _                 _                 _ = error "acidUpdate: state not open"

    -- Evaluating the update directly on the model value of the old
    -- state should give the same result
    ensure model _ (AcidCommand c _) o = case modelValue model of
      Just v  -> evalState (lookupMethod c) v === o
      Nothing -> failure

-- | Translate an acid-state query into a command that executes the
-- query, given a generator of inputs.
acidQuery :: forall s e m .
              ( Acid.IsAcidic s
              , Acid.EventState e ~ s
              , Acid.QueryEvent e
              , Show e
              , Eq (Acid.EventResult e)
              , Show (Acid.EventResult e)
              , Typeable (Acid.EventResult e)
              , MonadIO m
              , Eq s
              , Show s
              )
           => Gen e -> Command Gen m (Model s)
acidQuery gen_event = Command gen execute
   [ Require require
   , Ensure unchanged_model
   , Ensure correct_output
   ]
  where
    -- Generate queries only when state is open
    gen model = case modelHandle model of
                  Just st -> Just (AcidCommand <$> gen_event <*> pure st)
                  Nothing -> Nothing

    -- Execute a concrete query directly using acid-state
    execute (AcidCommand e (Var (Concrete (Opaque st)))) = liftIO (Acid.query st e)

    -- Shrinking queries requires the state to be open
    require model _ = isOpen model

    -- Queries should not change the value in the model
    unchanged_model model0 model1 _ _ = modelValue model0 === modelValue model1

    -- Evaluating the query directly on the model value of the old
    -- state should give the same result
    correct_output model _ (AcidCommand c _) o = case modelValue model of
      Just v  -> evalState (lookupMethod c) v === o
      Nothing -> failure


-- | Test the sequential property (agreement between model and
-- implementation) for an acid-state component, given the interface, a
-- generator for initial values of the state, and a list of commands
-- built from 'acidQuery' and 'acidUpdate'.  Additional commands will
-- be added to open and close the state.
--
-- Note that if the generator for initial values can return more than
-- one result, this will fail due to #20.
acidStateSequentialProperty :: AcidStateInterface s -> Gen s -> Range Int -> [Command Gen (TestT IO) (Model s)] -> Property
acidStateSequentialProperty i gen_initial_state range commands = property $ do
    actions <- forAll $ Gen.sequential range StateAbsent $
                 [ open i gen_initial_state
                 , close i
                 , checkpoint i
                 , checkpointClose i
                 , kill i
                 ] ++ commands ++ commands
    test $ do liftIO $ resetState i
              executeSequential StateAbsent actions

-- | Test the parallel property (absence of race conditions) for an
-- acid-state component, given the interface, a generator for initial
-- values of the state, and a list of commands built from 'acidQuery'
-- and 'acidUpdate'.  Additional commands will be added to open and
-- close the state.
--
-- Note that if the generator for initial values can return more than
-- one result, this will fail due to #20.
acidStateParallelProperty :: AcidStateInterface s -> Gen s -> Range Int -> Range Int -> [Command Gen (TestT IO) (Model s)] -> Property
acidStateParallelProperty i gen_initial_state prefix_range parallel_range commands = property $ do
    actions <- forAll $ Gen.parallel prefix_range parallel_range StateAbsent $
                 [ open i gen_initial_state
                 , checkpoint i
                 ] ++ commands
    test $ do liftIO $ resetState i
              executeParallel StateAbsent actions

-- | Test that restoring an acid-state component (with the given initial
-- value), then executing the given query, results in the given
-- expected value.  This is mostly useful to test that restoring from
-- files created by an older version of acid-state can still be read.
restoreOldStateProperty :: (Acid.EventState e ~ s, Acid.EventResult e ~ r, Acid.QueryEvent e, Eq r, Show r)
                        => AcidStateInterface s -> s -> e -> r -> Property
restoreOldStateProperty i initial_state q expected_result = withTests 1 $ property $ test $ do
    liftIO $ removeFile (statePath i ++ "/open.lock") `catch` (\ (_ :: IOException) -> return ())
    st <- liftIO $ openState i initial_state
    r  <- liftIO $ Acid.query st q
    r === expected_result
