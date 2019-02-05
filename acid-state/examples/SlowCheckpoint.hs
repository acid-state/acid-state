{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- With optimizations enabled, serializing the checkpoint can happen too quickly
{-# OPTIONS_GHC -O0 #-}

module SlowCheckpoint (main) where

import           Data.Acid

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           Data.Time
import           System.Directory
import           System.IO

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data SlowCheckpoint = SlowCheckpoint Int Int

$(deriveSafeCopy 0 'base ''SlowCheckpoint)

------------------------------------------------------
-- The transaction we will execute over the state.

-- This transaction adds a very computationally heavy entry
-- into our state. However, since the state is lazy, the
-- chunk will not be forced until we create a checkpoint.
-- Computing 'last [0..100000000]' takes roughly 2 seconds
-- on my machine.       XXX Lemmih, 2011-04-26
setComputationallyHeavyData :: Update SlowCheckpoint ()
setComputationallyHeavyData = do SlowCheckpoint _slow tick <- get
                                 put $ SlowCheckpoint (last [0..100000000]) tick

tick :: Update SlowCheckpoint Int
tick = do SlowCheckpoint slow tick <- get
          put $ SlowCheckpoint slow (tick+1)
          return tick

askTick :: Query SlowCheckpoint Int
askTick = do SlowCheckpoint _ tick <- ask
             return tick

$(makeAcidic ''SlowCheckpoint ['setComputationallyHeavyData, 'tick, 'askTick])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do putStrLn "SlowCheckpoint test"
          exists <- doesDirectoryExist fp
          when exists $ removeDirectoryRecursive fp
          acid <- openLocalStateFrom fp (SlowCheckpoint 0 0)
          putStrLn "This example illustrates that the state is still accessible while"
          putStrLn "a checkpoint is being serialized. This is an important property when"
          putStrLn "the size of a checkpoint reaches several hundred megabytes."
          putStrLn "If you don't see any ticks while the checkpoint is being created, something"
          putStrLn "has gone awry."
          putStrLn ""
          doTick acid
          update acid SetComputationallyHeavyData
          forkIO $ do putStrLn "Serializing checkpoint..."
                      t <- timeIt $ createCheckpoint acid
                      n <- query acid AskTick
                      putStrLn $ "Checkpoint created in: " ++ show t ++ " (saw " ++ show n ++ " ticks)"
                      when (n < threshold) $ error $ "Not enough ticks! Expected at least " ++ show threshold
          replicateM_ 20 $
            do doTick acid
               threadDelay (10^5)
          putStrLn "SlowCheckpoint done"
  where
    fp = "state/SlowCheckpoint"

    -- We must see at least this many ticks for the test to be considered a success
    threshold = 5

doTick acid
    = do tick <- update acid Tick
         putStrLn $ "Tick: " ++ show tick

timeIt action
    = do t1 <- getCurrentTime
         ret <- action
         t2 <- getCurrentTime
         return (diffUTCTime t2 t1)
