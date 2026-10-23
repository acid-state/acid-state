{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests that 'createCheckpointAndClose' does not write a checkpoint
-- identical to one this process has already made durable.
module Data.Acid.CheckpointSpec where

import Test.Hspec

import Control.Exception (bracket_)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.SafeCopy (base, deriveSafeCopy)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import Data.Acid
import Data.Acid.Local (Checkpoint, createCheckpointAndClose,
                        defaultSerialisationLayer, mkCheckpointsLogKey)
import Data.Acid.Log (FileLog, askCurrentEntryId, closeFileLog, openFileLog)

newtype Counter = Counter Int

$(deriveSafeCopy 0 'base ''Counter)

increment :: Update Counter ()
increment = modify (\(Counter n) -> Counter (n + 1))

current :: Query Counter Int
current = do Counter n <- ask
             return n

$(makeAcidic ''Counter ['increment, 'current])

spec :: Spec
spec = around withStateDir $
    describe "createCheckpointAndClose" $ do
        it "writes a checkpoint when events were logged" $ \dir -> do
            n <- closeAfter dir $ \acid -> update acid Increment
            n `shouldBe` 1
            valueIn dir `shouldReturn` 1

        it "writes a checkpoint for a fresh state with no events" $ \dir -> do
            n <- closeAfter dir $ \_ -> return ()
            n `shouldBe` 1
            -- The initial value must have been saved, so a different
            -- initial value at reopen is ignored.
            reopened <- openLocalStateFrom dir (Counter 42)
            query reopened Current `shouldReturn` 0
            closeAcidState reopened

        it "rewrites the checkpoint it loaded, even with no new events" $ \dir -> do
            _ <- closeAfter dir $ \acid -> update acid Increment
            n <- closeAfter dir $ \_ -> return ()
            -- The loaded checkpoint may need a SafeCopy migration, so
            -- closing must still write one.
            n `shouldBe` 1
            valueIn dir `shouldReturn` 1

        it "skips the checkpoint right after createCheckpoint" $ \dir -> do
            n <- closeAfter dir $ \acid -> do
                update acid Increment
                createCheckpoint acid
            -- Only the explicit checkpoint; the close wrote nothing.
            n `shouldBe` 1
            valueIn dir `shouldReturn` 1

        it "skips the checkpoint after repeated checkpoints with no events" $ \dir -> do
            n <- closeAfter dir $ \acid -> do
                update acid Increment
                createCheckpoint acid
                createCheckpoint acid
            n `shouldBe` 2
            valueIn dir `shouldReturn` 1

        it "writes a checkpoint when events follow the last checkpoint" $ \dir -> do
            n <- closeAfter dir $ \acid -> do
                update acid Increment
                createCheckpoint acid
                update acid Increment
            n `shouldBe` 2
            valueIn dir `shouldReturn` 2

        it "keeps the state recoverable across repeated skipped closes" $ \dir -> do
            let cycle_ = closeAfter dir $ \acid -> do
                    update acid Increment
                    createCheckpoint acid
            counts <- sequence [cycle_, cycle_, cycle_]
            counts `shouldBe` [1, 1, 1]
            valueIn dir `shouldReturn` 3

-- | Open the state in @dir@, run the action, then 'createCheckpointAndClose'.
-- Returns the number of checkpoints written while the state was open.
closeAfter :: FilePath -> (AcidState Counter -> IO ()) -> IO Int
closeAfter dir action = do
    countBefore <- checkpointCount dir
    acid <- openLocalStateFrom dir (Counter 0)
    action acid
    createCheckpointAndClose acid
    countAfter <- checkpointCount dir
    return (countAfter - countBefore)

valueIn :: FilePath -> IO Int
valueIn dir = do
    acid <- openLocalStateFrom dir (Counter 0)
    n <- query acid Current
    closeAcidState acid
    return n

-- | Number of entries in the checkpoint log.  Only valid while the state
-- is closed.
checkpointCount :: FilePath -> IO Int
checkpointCount dir = do
    fLog <- openFileLog (mkCheckpointsLogKey dir defaultSerialisationLayer)
              :: IO (FileLog (Checkpoint Counter))
    n <- askCurrentEntryId fLog
    closeFileLog fLog
    return n

withStateDir :: (FilePath -> IO ()) -> IO ()
withStateDir action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "acid-state-checkpoint-spec"
    bracket_ (removePathForcibly dir) (removePathForcibly dir) (action dir)
