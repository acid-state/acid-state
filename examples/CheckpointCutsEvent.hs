{-
This example is mostly just to test that this bug is fixed:

https://github.com/acid-state/acid-state/issues/73

At the end of a run, the checkpoint file should contain a single
checkpoint and the event file should be empty. The old checkpoints and
events should be in the Archive directory.

In the Acrhive directory, each checkpoint file should contain one
checkpoint, and each event file should contain 10 events.

If you comment out the 'createArchive' line below, then the checkpoint
files should contain 10 checkpoints each.

-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CheckpointCutsEvent (main) where

-- import           Control.Concurrent
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.List
import           Data.SafeCopy
import           Data.Typeable
import           System.Directory
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

newtype Counter = Counter { unCounter :: Integer }
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Counter)

incCounter :: Update Counter Integer
incCounter =
  do (Counter c) <- get
     let c' = succ c
     put (Counter c')
     return c'

$(makeAcidic ''Counter ['incCounter])


main :: IO ()
main = do
     putStrLn "CheckpointCutsEvent test"
     exists <- doesDirectoryExist fp
     when exists $ removeDirectoryRecursive fp
     acid <- openLocalStateFrom fp (Counter 0)
     replicateM_ 10 $ do is <- replicateM 10 (update acid IncCounter)
                         print is
                         createCheckpoint acid
                         createArchive acid
     closeAcidState acid
     checkDirectoryContents fp expected_state
     checkDirectoryContents (fp ++ "/Archive") expected_archive
     s <- readFile (fp ++ "/events-0000000100.log")
     unless (s == "") $ error "non-empty events file"
     putStrLn "CheckpointCutsEvent done"
  where
    fp = "state/CheckpointCutsEvent"

    expected_state   = [".","..","Archive","checkpoints-0000000009.log","checkpoints-0000000010.log"
                       ,"checkpoints.version","events-0000000100.log","events.version","open.lock"]
    expected_archive = [".","..","checkpoints-0000000000.log","checkpoints-0000000001.log"
                       ,"checkpoints-0000000002.log","checkpoints-0000000003.log","checkpoints-0000000004.log"
                       ,"checkpoints-0000000005.log","checkpoints-0000000006.log","checkpoints-0000000007.log"
                       ,"checkpoints-0000000008.log","events-0000000000.log","events-0000000010.log"
                       ,"events-0000000020.log","events-0000000030.log","events-0000000040.log"
                       ,"events-0000000050.log","events-0000000060.log","events-0000000070.log"
                       ,"events-0000000080.log","events-0000000090.log"]


checkDirectoryContents :: FilePath -> [FilePath] -> IO ()
checkDirectoryContents fp expected_fs = do
    putStrLn $ "Checking contents of " ++ fp
    fs <- sort <$> getDirectoryContents fp
    unless (fs == expected_fs) $ error $ "bad contents of " ++ fp ++ ": expected "
                                         ++ show expected_fs ++ " but got " ++ show fs
