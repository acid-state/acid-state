module Main (main) where

import           Control.Monad (unless)
import           Data.Acid.KeyValueStateMachine
import           System.Exit (exitFailure)

main :: IO ()
main = do ok <- tests
          unless ok exitFailure
