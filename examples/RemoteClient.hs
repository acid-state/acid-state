{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid.Remote

import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import System.IO
import Data.SafeCopy
import Data.Typeable
import Network

import RemoteCommon

------------------------------------------------------
-- This is how AcidState is used:

open :: IO (AcidState StressState)
open = openRemote "localhost" (PortNumber 8080)

main :: IO ()
main = do args <- getArgs
          case args of
            ["checkpoint"]
              -> do acid <- open 
                    createCheckpoint acid
            ["query"]
              -> do acid <- open
                    n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke"]
              -> do acid <- open
                    putStr "Issuing 100k transactions... "
                    hFlush stdout
                    replicateM_ (100000-1) (scheduleUpdate acid PokeState)
                    update acid PokeState
                    putStrLn "Done"
            ["clear"]
              -> do acid <- open
                    update acid ClearState
                    createCheckpoint acid
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  query            Prints out the current state."
                    putStrLn $ "  poke             Spawn 100k transactions."
                    putStrLn $ "  checkpoint       Create a new checkpoint."
