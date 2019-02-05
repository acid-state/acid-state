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
    , openLocalStateWithSerialiser
    , prepareLocalState
    , prepareLocalStateFrom
    , prepareLocalStateWithSerialiser
    , defaultStateDirectory
    , scheduleLocalUpdate'
    , scheduleLocalColdUpdate'
    , createCheckpointAndClose
    , LocalState(..)
    , Checkpoint(..)
    , SerialisationLayer(..)
    , defaultSerialisationLayer
    ) where

import Data.Acid.Archive
import Data.Acid.Core
import Data.Acid.Common
import Data.Acid.Internal.Local
import Data.Acid.Abstract

import Control.Applicative            ( (<$>), (<*>) )

import Data.Serialize                 ( runPutLazy, runGetLazy )
import Data.SafeCopy                  ( SafeCopy(..), safeGet, safePut
                                      , primitive, contain )
import Data.Typeable                  ( Typeable )

-- | Previous versions of @acid-state@ had
--
-- > data Checkpoint = Checkpoint EntryId ByteString
--
-- where the 'ByteString' is the @safecopy@-serialization of the
-- original checkpoint data.  Thus we give a 'SafeCopy' instance that
-- is backwards-compatible with this by making nested calls to
-- 'safePut' and 'safeGet'.
--
-- Note that if the inner data cannot be deserialised, 'getCopy' will
-- not report an error immediately but will return a 'Checkpoint'
-- whose payload is an error thunk.  This means consumers can skip
-- deserialising intermediate checkpoint data when they care only
-- about the last checkpoint in a file.  However, they must be sure to
-- force the returned data promptly.
instance SafeCopy s => SafeCopy (Checkpoint s) where
    kind = primitive
    putCopy (Checkpoint eventEntryId content)
        = contain $
          do safePut eventEntryId
             safePut (runPutLazy (safePut content))
    getCopy = contain $ Checkpoint <$> safeGet <*> (fromNested <$> safeGet)
      where
        fromNested b = case runGetLazy safeGet b of
                         Left msg -> checkpointRestoreError msg
                         Right v  -> v

checkpointRestoreError msg
    = error $ "Could not parse saved checkpoint due to the following error: " ++ msg


-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
openLocalState :: (Typeable st, IsAcidic st, SafeCopy st)
              => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                             --   found.
              -> IO (AcidState st)
openLocalState initialState =
  openLocalStateFrom (defaultStateDirectory initialState) initialState

-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalState :: (Typeable st, IsAcidic st, SafeCopy st)
                  => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                                 --   found.
                  -> IO (IO (AcidState st))
prepareLocalState initialState =
  prepareLocalStateFrom (defaultStateDirectory initialState) initialState

-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   Running two AcidState's from the same directory is an error
--   but will not result in dataloss.
openLocalStateFrom :: (IsAcidic st, SafeCopy st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (AcidState st)
openLocalStateFrom directory initialState =
  openLocalStateWithSerialiser directory initialState defaultSerialisationLayer

-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalStateFrom :: (IsAcidic st, SafeCopy st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (IO (AcidState st))
prepareLocalStateFrom directory initialState =
  prepareLocalStateWithSerialiser directory initialState defaultSerialisationLayer


-- | Standard (and historically the only) serialisation layer, using
-- 'safeCopySerialiser' and 'defaultArchiver'.
defaultSerialisationLayer :: SafeCopy st => SerialisationLayer st
defaultSerialisationLayer = SerialisationLayer safeCopySerialiser safeCopySerialiser defaultArchiver
