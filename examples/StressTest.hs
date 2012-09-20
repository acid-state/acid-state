{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid
import Data.Acid.Advanced (groupUpdates)

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

clearState :: Update StressState ()
clearState = put $ StressState 0

$(makeAcidic ''StressState ['pokeState, 'queryState, 'clearState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          acid <- openLocalState (StressState 0)
          case args of
            ["checkpoint"]
              -> createCheckpoint acid
            ["query"]
              -> do n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke"]
              -> do putStr "Issuing 100k transactions... "
                    hFlush stdout
                    groupUpdates acid (replicate 100000 PokeState)
                    putStrLn "Done"
            ["clear"]
              -> do update acid ClearState
                    createCheckpoint acid
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  query            Prints out the current state."
                    putStrLn $ "  poke             Spawn 100k transactions."
                    putStrLn $ "  checkpoint       Create a new checkpoint."
