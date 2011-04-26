{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid
import Data.Acid.Local

import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import System.IO
import Data.SafeCopy

import Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data StressState = StressState !Int
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''StressState)

------------------------------------------------------
-- The transaction we will execute over the state.

pokeState :: Update StressState ()
pokeState = do StressState i <- get
               put (StressState (i+1))

queryState :: Query StressState Int
queryState = do StressState i <- ask
                return i

$(makeAcidic ''StressState ['pokeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          case args of
            ["checkpoint"]
              -> do acid <- openAcidState (StressState 0)
                    createCheckpoint acid
            ["query"]
              -> do acid <- openAcidState (StressState 0)
                    n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke"]
              -> do acid <- openAcidState (StressState 0)
                    putStr "Issuing 100k transactions... "
                    hFlush stdout
                    replicateM_ (100000-1) (scheduleUpdate acid PokeState)
                    update acid PokeState
                    putStrLn "Done"
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  query            Prints out the current state."
                    putStrLn $ "  poke             Spawn 100k transactions."
                    putStrLn $ "  checkpoint       Create a new checkpoint."
