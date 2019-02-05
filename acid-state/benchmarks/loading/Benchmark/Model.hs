module Benchmark.Model where

import Benchmark.Prelude hiding (insert)
import qualified Data.Acid as Acid



type Model = [[[Int]]]

insert :: [[Int]] -> Acid.Update Model ()
insert = modify . (:)

sumUp :: Acid.Query Model Int
sumUp = sum . map (sum . map sum) <$> ask

Acid.makeAcidic ''Model ['insert, 'sumUp]
