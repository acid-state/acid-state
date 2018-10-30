{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides a general framework for state-machine testing of
-- acid-state components.  It needs to be instantiated with a
-- particular acid-state component to get a concrete test.
module Data.Acid.StateMachineTest
  ( acidUpdate
  , acidUpdateMayFail
  , acidUpdateCheckFail
  , acidQuery
  , acidQueryMayFail
  , acidQueryCheckFail
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

    -- * Testing exceptions
  , TransactionError(..)
  , failQuery
  , failUpdate
  , Bomb(..)
  , explodeWHNF
  , explodeNF
  , genBomb
  ) where

import           Control.Exception (IOException, catch)
import qualified Data.Acid as Acid
import qualified Data.Acid.Local as Local
import           Data.Acid.Test.StateMachine
import qualified Data.SafeCopy as SafeCopy
import           Data.Typeable
import           System.Directory (removeDirectoryRecursive)

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''Bomb)

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
