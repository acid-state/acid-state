module Main where

import Data.Acid.Repair
import System.Directory

main :: IO ()
main = do
    directory <- getCurrentDirectory
    repairEvents directory
    repairCheckpoints directory
