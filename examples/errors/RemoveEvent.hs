{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module RemoveEvent (main, test) where

import           Data.Acid

import           Control.Monad
import           Data.SafeCopy
import           System.Directory
import           System.Environment
import           Data.List (isSuffixOf)

import           Data.Typeable

import           Control.Exception
import           Prelude             hiding (catch)

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data FirstState = FirstState
    deriving (Show)

data SecondState = SecondState
    deriving (Show)

$(deriveSafeCopy 0 'base ''FirstState)
$(deriveSafeCopy 0 'base ''SecondState)

------------------------------------------------------
-- The transaction we will execute over the state.

firstEvent :: Update FirstState ()
firstEvent = return ()

$(makeAcidic ''FirstState ['firstEvent])
$(makeAcidic ''SecondState [])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do putStrLn "This example simulates what happens when you remove an event"
          putStrLn "that is required to replay the journal."
          putStrLn "Hopefully this program will fail with a readable error message."
          putStrLn ""
          firstAcid <- openLocalStateFrom fp FirstState
          update firstAcid FirstEvent
          closeAcidState firstAcid

          secondAcid <- openLocalStateFrom fp SecondState
          closeAcidState secondAcid
          error "If you see this message then something has gone wrong!"

test :: IO ()
test = do
    putStrLn "RemoveEvent test"
    exists <- doesDirectoryExist fp
    when exists $ removeDirectoryRecursive fp
    handle hdl main
    putStrLn "RemoveEvent done"
  where
    hdl (ErrorCall msg)
      | "This method is required but not available: \"RemoveEvent.FirstEvent\". Did you perhaps remove it before creating a checkpoint?" `isSuffixOf` msg
      = putStrLn $ "Caught error: " ++ msg
    hdl e = throwIO e

fp = "state/RemoveEvent"
