{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module MonadStateConstraint (main) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data HelloWorldState = HelloWorldState String
    deriving (Show)

$(deriveSafeCopy 0 'base ''HelloWorldState)

------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: MonadState HelloWorldState m => String -> m ()
writeState newValue
    = put (HelloWorldState newValue)

queryState :: MonadReader HelloWorldState m => m String
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
                     putStrLn "The state has been modified!"
