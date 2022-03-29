{-# LANGUAGE RecordWildCards #-}

import Benchmark.Prelude
import Criterion.Main
import qualified Data.Acid as Acid
import Benchmark.Interface
import qualified Benchmark.FileSystem as FS
import qualified System.Random as Random


main :: IO ()
main = defaultMain [benchmark defaultBenchmarkInterfaces [100,200,300,400]]

benchmark :: [BenchmarkInterface] -> [Int] -> Benchmark
benchmark bis sizes = env setupWorkingPath $ \ workingPath ->
    bgroup "" $ map (benchmarksGroup workingPath sizes) bis


setupWorkingPath :: IO FS.FilePath
setupWorkingPath = do
  workingPath <- do workingPath <- FS.getTemporaryDirectory
                    rndStr <- replicateM 16 $ Random.randomRIO ('a', 'z')
                    return $ workingPath <> "acid-state" <> "benchmarks" <> "loading" <> FS.decodeString rndStr
  putStrLnDebug $ "Working under the following temporary directory: " ++ FS.encodeString workingPath
  FS.removeTreeIfExists workingPath
  FS.createTree workingPath
  return workingPath


benchmarksGroup :: FS.FilePath -> [Int] -> BenchmarkInterface -> Benchmark
benchmarksGroup workingPath sizes bi =
  bgroup (benchName bi)
  [ bgroup (show size) $
      initializeBenchmarksGroup bi workingPath' size
    : specialCaseBenchmarksGroup bi workingPath' size
    : if benchPersist bi then [openCloseBenchmarksGroup  bi workingPath' size] else []
  | size <- sizes
  , let workingPath' = workingPath <> FS.decodeString (show size)
  ]


-- | The Initialize benchmarks measure how long it takes to open an
-- empty 'AcidState' component, call 'initialize' to populate it with
-- data, and optionally checkpoint before closing.
initializeBenchmarksGroup :: BenchmarkInterface -> FS.FilePath -> Int -> Benchmark
initializeBenchmarksGroup bi workingPath size =
  env (evaluate (mkStateValue 100 100)) $ \ state_value ->
  bgroup "Initialize"
  [ bench "Without checkpoint" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
      initializeClose bi workingPath size state_value
  , bench "With checkpoint" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
      initializeCheckpointClose bi workingPath size state_value
  ]

-- | These benchmarks perform the same actions as initialize but adapted to
-- cover two important special cases: many small events, and one checkpoint of a
-- large value.
specialCaseBenchmarksGroup :: BenchmarkInterface -> FS.FilePath -> Int -> Benchmark
specialCaseBenchmarksGroup bi workingPath size =
  bgroup "Special"
  [ bench "Small events" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
        initializeClose bi workingPath size (mkStateValue 0 0)

  , env (evaluate (mkStateValue size 50000)) $ \state_value ->
      bench "Large checkpoint" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
        initializeCheckpointClose bi workingPath 1 state_value
  ]

prepareInitialize :: BenchmarkInterface -> FS.FilePath -> IO ()
prepareInitialize bi workingPath =
    when (benchPersist bi) $ do FS.removeTreeIfExists workingPath
                                FS.createTree workingPath


-- | The OpenClose benchmarks measure how long it takes to open an
-- existing on-disk 'AcidState' component (either from a checkpoint or
-- from a transaction log), optionally execute a query over the entire
-- state, then close.  These benchmarks are not applicable if the
-- interface being benchmarked does not persist data.
openCloseBenchmarksGroup :: BenchmarkInterface -> FS.FilePath -> Int -> Benchmark
openCloseBenchmarksGroup bi workingPath size =
  env (prepareOpenCloseBenchmarksGroup bi workingPath size) $ \ ~(logsInstancePath, checkpointInstancePath) -> bgroup "OpenClose"
    [
      bench "From Logs" $ nfIO $
        openClose bi logsInstancePath
    , bench "From Checkpoint" $ nfIO $
        openClose bi checkpointInstancePath
    , bench "From Logs (with query)" $ nfIO $
        openQueryClose bi logsInstancePath
    , bench "From Checkpoint (with query)" $ nfIO $
        openQueryClose bi checkpointInstancePath
    ]

-- | Set up data on disk for the open/close benchmarks.  This
-- initializes an instance, creates a copy of it (for restoring from
-- transaction logs), then checkpoints.
prepareOpenCloseBenchmarksGroup :: BenchmarkInterface -> FS.FilePath -> Int -> IO (FS.FilePath, FS.FilePath)
prepareOpenCloseBenchmarksGroup bi workingPath size = do
  putStrLnDebug $ "Preparing instances for size " ++ show size

  let
    logsInstancePath = workingPath <> "logs-instance"
    checkpointInstancePath = workingPath <> "checkpoint-instance"

  FS.createTree logsInstancePath
  FS.createTree checkpointInstancePath

  putStrLnDebug "Initializing"
  initialize bi checkpointInstancePath size (mkStateValue 100 100) $ \inst -> do

    putStrLnDebug "Copying"
    FS.copy checkpointInstancePath logsInstancePath
    FS.removeFile $ logsInstancePath <> "open.lock"

    putStrLnDebug "Checkpointing"
    Acid.createCheckpoint inst

    putStrLnDebug "Closing"
    Acid.closeAcidState inst

    return (logsInstancePath, checkpointInstancePath)


initialize :: BenchmarkInterface -> FS.FilePath -> Int -> StateValue -> (forall m . Acid.AcidState m -> IO r) -> IO r
initialize BenchmarkInterface{..} p size value k = do
    inst <- benchOpen p
    replicateM_ size (benchUpdate inst value)
    k inst

initializeClose :: BenchmarkInterface -> FS.FilePath -> Int -> StateValue -> IO ()
initializeClose bi p size value = initialize bi p size value Acid.closeAcidState

initializeCheckpointClose :: BenchmarkInterface -> FS.FilePath -> Int -> StateValue -> IO ()
initializeCheckpointClose bi p size value =
  initialize bi p size value $ \ inst -> do
    Acid.createCheckpoint inst
    Acid.closeAcidState inst


openClose :: BenchmarkInterface -> FS.FilePath -> IO ()
openClose BenchmarkInterface{..} p = benchOpen p >>= Acid.closeAcidState

openQueryClose :: BenchmarkInterface -> FS.FilePath -> IO Int
openQueryClose BenchmarkInterface{..} p = do
    inst <- benchOpen p
    n <- benchQuery inst
    Acid.closeAcidState inst
    return n


putStrLnDebug :: String -> IO ()
-- putStrLnDebug = putStrLn
putStrLnDebug _ = pure ()
