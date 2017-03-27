{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           Data.Time
import           System.IO
import           System.Random

data App = App Int

$(deriveSafeCopy 0 'base ''App)

getApp :: Query App Int
getApp = do
  App i <- ask
  return i

-- | write new value to state.  return old value.
setApp :: Int -> Update App ()
setApp = put . App

$(makeAcidic ''App ['getApp, 'setApp])


-- | not atomic: write random value to state; read value from state
-- and make sure that it's the value we just wrote.
brokenCheck :: AcidState App -> IO ()
brokenCheck st = do
  i <- randomRIO (0, 100)
  update st $ SetApp i
  i' <- query st $ GetApp
  assert (i == i') $ return ()

-- | atomic, but clumsy: variant of 'brokenCheck'.
clumsyCheck :: MVar () -> AcidState App -> IO ()
clumsyCheck mvar st = do
  takeMVar mvar
  i <- randomRIO (0, 100)
  update st $ SetApp i
  i' <- query st $ GetApp
  putMVar mvar ()
  assert (i == i') $ return ()


doCheck :: MVar () -> AcidState App -> IO ()
doCheck mvar acidState = f  where f = clumsyCheck mvar acidState >> f


main :: IO ()
main = do acidState <- openLocalStateFrom "state/App" (App 0)
          putStrLn "This example shows how acidity only holds for events."
          putStrLn ""
          mvar <- newMVar ()
          mapM_ (forkIO . doCheck mvar . const acidState) [1..100]
