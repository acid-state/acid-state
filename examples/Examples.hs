module Main (main) where

import qualified CheckpointCutsEvent
import qualified Exceptions
import qualified RemoveEvent
import qualified SlowCheckpoint

main :: IO ()
main = do
  CheckpointCutsEvent.main
  Exceptions.test
  RemoveEvent.test
  SlowCheckpoint.main
