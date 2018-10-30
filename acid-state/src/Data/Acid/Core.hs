{-# LANGUAGE CPP, GADTs, DeriveDataTypeable, TypeFamilies,
             FlexibleContexts, BangPatterns,
             DefaultSignatures, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Core
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- Low-level controls for transaction-based state changes. This module defines
-- structures and tools for running state modifiers indexed either by an Method
-- or a serialized Method. This module should rarely be used directly although
-- the 'Method' class is needed when defining events manually.
--
-- The term \'Event\' is loosely used for transactions with ACID guarantees.
-- \'Method\' is loosely used for state operations without ACID guarantees
--
module Data.Acid.Core
    ( Core(coreMethods)
    , Method(..)
    , MethodContainer(..)
    , Tagged
    , mkCore
    , closeCore
    , closeCore'
    , modifyCoreState
    , modifyCoreState_
    , withCoreState
    , lookupHotMethod
    , lookupHotMethodAndSerialiser
    , lookupColdMethod
    , runHotMethod
    , runColdMethod
    , MethodMap
    , mkMethodMap

    , Serialiser(..)
    , safeCopySerialiser
    , MethodSerialiser(..)
    , safeCopyMethodSerialiser
    , encodeMethod
    , decodeMethod
    , encodeResult
    , decodeResult
    ) where

import Data.Acid.Internal.Core

import Data.Serialize                     ( runPutLazy, runGetLazy )
import Data.SafeCopy                      ( SafeCopy, safeGet, safePut )

-- | Default implementation of 'Serialiser' interface using 'SafeCopy'.
safeCopySerialiser :: SafeCopy a => Serialiser a
safeCopySerialiser = Serialiser (runPutLazy . safePut) (runGetLazy safeGet)

-- | Default implementation of 'MethodSerialiser' interface using 'SafeCopy'.
safeCopyMethodSerialiser :: (SafeCopy method, SafeCopy (MethodResult method)) => MethodSerialiser method
safeCopyMethodSerialiser = MethodSerialiser safeCopySerialiser safeCopySerialiser
