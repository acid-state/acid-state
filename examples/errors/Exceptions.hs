{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Exceptions (main, test) where

import           Data.Acid
import           Data.Acid.Local     (createCheckpointAndClose)

import           Control.Monad
import           Control.Monad.State ( get, put )
import           Data.SafeCopy
import           System.Directory
import           System.Environment

import           Data.Typeable

import           Control.Exception
import           Prelude             hiding (catch)

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data MyState = MyState Integer
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''MyState)

------------------------------------------------------
-- The transaction we will execute over the state.

failEvent :: Update MyState ()
failEvent = pure $ error "fail!"

errorEvent :: Update MyState ()
errorEvent = error "error!"

stateError :: Update MyState ()
stateError = put (error "state error!")

stateNestedError1 :: Update MyState ()
stateNestedError1 = put (MyState (error "nested state error (1)"))

stateNestedError2 :: Integer -> Update MyState ()
stateNestedError2 n = put (MyState n)

tick :: Update MyState Integer
tick = do MyState n <- get
          put $ MyState (n+1)
          return n

$(makeAcidic ''MyState [ 'failEvent
                       , 'errorEvent
                       , 'stateError
                       , 'stateNestedError1
                       , 'stateNestedError2
                       , 'tick
                       ])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openLocalStateFrom "state/Exceptions" (MyState 0)
          args <- getArgs
          case args of
            ["1"] -> update acid (undefined :: FailEvent)
            ["2"] -> update acid FailEvent
            ["3"] -> update acid ErrorEvent
            ["4"] -> update acid StateError
            ["5"] -> update acid StateNestedError1
            ["6"] -> update acid (StateNestedError2 (error "nested state error (2)"))
            _     -> do putStrLn "Call with [123456] to test error scenarios."
                        putStrLn "If the tick doesn't get stuck, everything is fine."
                        do_tick acid
           `catch` \e -> do putStrLn $ "Caught exception: " ++ show (e:: SomeException)
                            createCheckpointAndClose acid

do_tick :: AcidState MyState -> IO ()
do_tick acid = do n <- update acid Tick
                  putStrLn $ "Tick: " ++ show n

test :: IO ()
test = do putStrLn "Exceptions test"
          exists <- doesDirectoryExist fp
          when exists $ removeDirectoryRecursive fp
          acid <- openLocalStateFrom fp (MyState 0)
          do_tick acid
          handle hdl $ update acid (undefined :: FailEvent)
          do_tick acid
          handle hdl $ update acid FailEvent
          do_tick acid
          handle hdl $ update acid ErrorEvent
          do_tick acid
          handle hdl $ update acid StateError
          do_tick acid
          -- We can't currently cope with an error being thrown during serialization of the state (see #38)
          -- handle hdl $ update acid StateNestedError1
          do_tick acid
          handle hdl $ update acid (StateNestedError2 (error "nested state error (2)"))
          do_tick acid
          n <- update acid Tick
          unless (n == expected_n) $ error $ "Wrong tick value, expected " ++ show expected_n
          createCheckpointAndClose acid
          putStrLn "Exceptions done"
  where
    fp = "state/Exceptions"

    hdl e = putStrLn $ "Caught exception: " ++ show (e:: SomeException)

    expected_n = 7
