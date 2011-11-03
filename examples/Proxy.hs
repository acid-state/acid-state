{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
module Main (main) where

import Data.Acid
import Data.Acid.Remote
import Data.Acid.Advanced   ( scheduleUpdate )

import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import System.IO
import Network
import Data.SafeCopy

import Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data ProxyStressState = StressState !Int
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''ProxyStressState)

------------------------------------------------------
-- The transaction we will execute over the state.

pokeState :: Update ProxyStressState ()
pokeState = do StressState i <- get
               put (StressState (i+1))

queryState :: Query ProxyStressState Int
queryState = do StressState i <- ask
                return i

clearState :: Update ProxyStressState ()
clearState = put $ StressState 0

$(makeAcidic ''ProxyStressState ['pokeState, 'queryState, 'clearState])

openLocal :: IO (AcidState ProxyStressState)
openLocal = openLocalState (StressState 0)

openRemote :: String -> IO (AcidState ProxyStressState)
openRemote socket = openRemoteState "localhost" (UnixSocket socket)

main :: IO ()
main = do args <- getArgs
          case args of
            ["server", socket]
              -> do acid <- openLocal
                    acidServer acid (UnixSocket socket)
            ["proxy", from, to] 
              -> do acid <- openRemote from
                    acidServer acid (UnixSocket to)
            ["query", socket]
              -> do acid <- openRemote socket
                    n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke", socket]
              -> do acid <- openRemote socket
                    putStr "Issuing 100k transactions... "
                    hFlush stdout
                    replicateM_ (100000-1) (scheduleUpdate acid PokeState)
                    update acid PokeState
                    putStrLn "Done"
            ["clear", socket]
              -> do acid <- openRemote socket
                    update acid ClearState
                    createCheckpoint acid
            ["checkpoint", socket]
              -> do acid <- openRemote socket
                    createCheckpoint acid
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  server socket      Start a new server instance."
                    putStrLn $ "  proxy from to      Pipe events between 'from' and 'to'."
                    putStrLn $ "  query socket       Prints out the current state."
                    putStrLn $ "  poke socket        Spawn 100k transactions."
                    putStrLn $ "  clear socket       Reset the state and write a checkpoint."
                    putStrLn $ "  checkpoint socket  Create a new checkpoint."
