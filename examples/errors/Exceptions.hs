{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid

import Control.Monad.State
import System.Environment
import Data.SafeCopy

import Data.Typeable

import Control.Exception
import Prelude hiding (catch)

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

newtype MyState = MyState Integer
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''MyState)

------------------------------------------------------
-- The transaction we will execute over the state.

failEvent :: Update MyState ()
failEvent = fail "fail!"

errorEvent :: Update MyState ()
errorEvent = error "error!"

stateError :: Update MyState ()
stateError = put (error "state error!")

tick :: Update MyState Integer
tick = do MyState n <- get
          put $ MyState (n+1)
          return n

$(makeAcidic ''MyState ['failEvent, 'errorEvent, 'stateError, 'tick])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openAcidState (MyState 0)
          args <- getArgs
          case args of
            ["1"] -> update acid (undefined :: FailEvent)
            ["2"] -> update acid FailEvent
            ["3"] -> update acid ErrorEvent
            ["4"] -> update acid ErrorEvent `catch` \e ->
                     putStrLn $ "Caught exception: " ++ show (e::SomeException)
            ["5"] -> update acid StateError
            _     -> do putStrLn "Call with '1', '2', '3', '4' or '5' to test error scenarios."
                        n <- update acid Tick
                        putStrLn $ "Tick: " ++ show n
