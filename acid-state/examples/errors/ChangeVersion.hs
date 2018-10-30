{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module ChangeVersion (main, test) where

import           Data.Acid

import           Control.Exception
import           Control.Monad.State
import           Data.SafeCopy
import           System.Directory
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data FirstState = FirstState String
    deriving (Show)

data SecondState = SecondState Int
    deriving (Show)

$(deriveSafeCopy 0 'base ''FirstState)
$(deriveSafeCopy 1 'base ''SecondState)
-- The version number difference is important here, as safecopy has no
-- way to reliably notice if we change a type and fail to update its
-- migration history.  In some cases the serialised data will fail to
-- parse (see the "ChangeState" example), but it depends on the types
-- involved, and this will not always be the case.

------------------------------------------------------
-- The transaction we will execute over the state.

$(makeAcidic ''FirstState [])
$(makeAcidic ''SecondState [])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do putStrLn "This example simulates what happens when you modify your safecopy"
          putStrLn "version without specifying how to migrate from the old version to the new."
          putStrLn "Hopefully this program will fail with a readable error message."
          putStrLn ""
          firstAcid <- openLocalStateFrom fp (FirstState "first state")
          createCheckpoint firstAcid
          closeAcidState firstAcid
          secondAcid <- openLocalStateFrom fp (SecondState 42)
          closeAcidState secondAcid
          error "If you see this message then something has gone wrong!"

test :: IO ()
test = do
    putStrLn "ChangeVersion test"
    exists <- doesDirectoryExist fp
    when exists $ removeDirectoryRecursive fp
    handle hdl main
    putStrLn "ChangeVersion done"
  where
    hdl (ErrorCall msg)
      | msg == "Could not parse saved checkpoint due to the following error: Failed reading: safecopy: ChangeVersion.SecondState: Cannot find getter associated with this version number: Version {unVersion = 0}\nEmpty call stack\n"
      = putStrLn $ "Caught error: " ++ msg
    hdl e = throwIO e

fp = "state/ChangeVersion"
