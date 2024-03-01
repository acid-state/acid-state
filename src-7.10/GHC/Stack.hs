-- | Fake HasCallStack constraint for ghc-7.10.

{-# LANGUAGE ConstraintKinds, ImplicitParams #-}

module GHC.Stack where

type HasCallStack = (?callStack :: ())
