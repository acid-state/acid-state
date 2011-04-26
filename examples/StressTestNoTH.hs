{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Main (main) where

import Data.Acid.Core
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

instance SafeCopy StressState where
    putCopy (StressState state) = contain $ safePut state
    getCopy = contain $ liftM StressState safeGet

------------------------------------------------------
-- The transaction we will execute over the state.

pokeState :: Update StressState ()
pokeState = do StressState i <- get
               put (StressState (i+1))

queryState :: Query StressState Int
queryState = do StressState i <- ask
                return i


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


------------------------------------------------------
-- The gritty details. These things may be done with
-- Template Haskell in the future.

data PokeState = PokeState
data QueryState = QueryState


deriving instance Typeable PokeState
instance SafeCopy PokeState where
    putCopy PokeState = contain $ return ()
    getCopy = contain $ return PokeState
instance Method PokeState where
    type MethodResult PokeState = ()
    type MethodState PokeState = StressState
instance UpdateEvent PokeState

deriving instance Typeable QueryState
instance SafeCopy QueryState where
    putCopy QueryState = contain $ return ()
    getCopy = contain $ return QueryState
instance Method QueryState where
    type MethodResult QueryState = Int
    type MethodState QueryState = StressState
instance QueryEvent QueryState

instance IsAcidic StressState where
    acidEvents = [ UpdateEvent (\PokeState -> pokeState)
                 , QueryEvent (\QueryState -> queryState)
                 ]
