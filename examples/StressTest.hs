{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid.Core
import Data.Acid

import qualified Control.Monad.State as State
import Control.Monad.Reader
import System.Environment
import System.IO
import Data.Serialize

import Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data StressState = StressState !Int
    deriving (Show, Typeable)

instance Serialize StressState where
    put (StressState state) = put state
    get = liftM StressState get

------------------------------------------------------
-- The transaction we will execute over the state.

pokeState :: Update StressState ()
pokeState = do StressState i <- State.get
               State.put (StressState (i+1))

queryState :: Query StressState Int
queryState = do StressState i <- ask
                return i

$(makeAcidic ''StressState ['pokeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openAcidState (StressState 0)
          args <- getArgs
          case args of
            ["checkpoint"]
              -> createCheckpoint acid
            ["query"]
              -> do n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke"]
              -> do putStr "Issuing 10k sequential transactions... "
                    hFlush stdout
                    replicateM_ 10000 (update acid PokeState)
                    putStrLn "Done"
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  query            Prints out the current state."
                    putStrLn $ "  poke             Spawn 10k transactions."
                    putStrLn $ "  checkpoint       Create a new checkpoint."
          closeAcidState acid
