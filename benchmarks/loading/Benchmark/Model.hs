{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Benchmark.Model where

import Benchmark.Prelude
import qualified Data.Acid as Acid



type Model = [[[Int]]]

insert :: [[Int]] -> Acid.Update Model ()
insert = modify . (:)

$(Acid.makeAcidic ''Model ['insert])


