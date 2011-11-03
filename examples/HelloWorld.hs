{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid

import Control.Monad.State
import Control.Monad.Reader
import System.Environment
import Data.SafeCopy

import Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data HelloWorldState = HelloWorldState String
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''HelloWorldState)

------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: String -> Update HelloWorldState ()
writeState newValue
    = put (HelloWorldState newValue)

queryState :: Query HelloWorldState String
queryState = do HelloWorldState string <- ask
                return string

$(makeAcidic ''HelloWorldState ['writeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openLocalState (HelloWorldState "Hello world")
          args <- getArgs
          if null args
             then do string <- query acid QueryState
                     putStrLn $ "The state is: " ++ string
             else do update acid (WriteState (unwords args))
                     putStrLn $ "The state has been modified!"
