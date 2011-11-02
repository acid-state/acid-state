{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid
import qualified Data.Acid.Memory.Pure as Pure
import qualified Data.Acid.Remote as Remote

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Environment
import System.IO
import System.Exit
import Network
import Data.SafeCopy

import Data.Typeable

import qualified Data.Map as Map

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = String
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          let port = 12345
              host = "localhost"
          case args of
            ["remote"]
              -> do putStrLn ("Acid server listening on port " ++ show port)
                    acid <- openAcidState (KeyValue Map.empty)
                    Remote.acidServer acid (PortNumber port)
            ["remote", key]
              -> do acid <- Remote.openRemote host (PortNumber port)
                    mbKey <- Remote.query acid (LookupKey key)
                    case mbKey of
                      Nothing    -> putStrLn $ key ++ " has no associated value."
                      Just value -> putStrLn $ key ++ " = " ++ value
                    exitWith ExitSuccess
            ["remote", key, val]
              -> do acid <- Remote.openRemote host (PortNumber port)
                    Remote.update acid (InsertKey key val)
                    putStrLn "Done."
                    exitWith ExitSuccess
            _ -> return ()
          acid <- openAcidState (KeyValue Map.empty)
          case args of
            [key]
              -> do mbKey <- query acid (LookupKey key)
                    case mbKey of
                      Nothing    -> putStrLn $ key ++ " has no associated value."
                      Just value -> putStrLn $ key ++ " = " ++ value
            [key,val]
              -> do update acid (InsertKey key val)
                    putStrLn "Done."
            _ -> do putStrLn "Usage:"
                    putStrLn "  key               Lookup the value of 'key'."
                    putStrLn "  key value         Set the value of 'key' to 'value'."
                    putStrLn "  remote            Start the server on port 12345."
                    putStrLn "  remote key        Connect to localhost:12345 and look for 'key'."
                    putStrLn "  remote key value  Connect to localhost:12345 and set 'key' = 'value'."
          closeAcidState acid
