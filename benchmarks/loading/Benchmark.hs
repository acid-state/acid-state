{-# LANGUAGE RecordWildCards #-}

import Benchmark.Prelude
import Criterion.Main
import qualified Data.Acid as Acid
import qualified Data.Acid.Memory as Memory
import qualified Benchmark.FileSystem as FS
import qualified Benchmark.Model as Model; import Benchmark.Model (Model)
import qualified System.Random as Random


main :: IO ()
main = defaultMain [benchmark defaultBenchmarkInterfaces [100,200,300,400]]

benchmark :: [BenchmarkInterface] -> [Int] -> Benchmark
benchmark bis sizes = env setupWorkingPath $ \ workingPath ->
    bgroup "" $ map (benchmarksGroup bis workingPath) sizes


-- | An acid-state interface to be benchmarked.
data BenchmarkInterface = forall m .
    BenchmarkInterface
        { benchName    :: String
          -- ^ Name of the interface, for use in constructing benchmarks.
        , benchPersist :: Bool
          -- ^ Does this interface actually persist data to disk? If
          -- it doesn't, some benchmarks are not applicable.
        , benchOpen    :: FS.FilePath -> IO (Acid.AcidState m)
          -- ^ Open an acid-state component with the given path.  Note
          -- that the type of the state is encapsulated within
          -- 'BenchmarkInterface'.
        , benchUpdate  :: Acid.AcidState m -> [[Int]] -> IO ()
          -- ^ Execute an 'insert' update against the acid-state.
        , benchQuery   :: Acid.AcidState m -> IO Int
          -- ^ Execute a 'sumUp' query against the state.
        }

memoryBenchmarkInterface :: BenchmarkInterface
memoryBenchmarkInterface =
    BenchmarkInterface { benchName    = "Memory"
                       , benchPersist = False
                       , benchOpen    = const $ Memory.openMemoryState mempty
                       , benchUpdate  = \ inst v -> Acid.update inst (Model.Insert v)
                       , benchQuery   = \ inst -> Acid.query inst Model.SumUp
                       }

localBenchmarkInterface :: BenchmarkInterface
localBenchmarkInterface =
    BenchmarkInterface { benchName    = "Local"
                       , benchPersist = True
                       , benchOpen    = \ p -> Acid.openLocalStateFrom (FS.encodeString p) mempty
                       , benchUpdate  = \ inst v -> Acid.update inst (Model.Insert v)
                       , benchQuery   = \ inst -> Acid.query inst Model.SumUp
                       }

defaultBenchmarkInterfaces :: [BenchmarkInterface]
defaultBenchmarkInterfaces = [memoryBenchmarkInterface, localBenchmarkInterface]


setupWorkingPath :: IO FS.FilePath
setupWorkingPath = do
  workingPath <- do workingPath <- FS.getTemporaryDirectory
                    rndStr <- replicateM 16 $ Random.randomRIO ('a', 'z')
                    return $ workingPath <> "acid-state" <> "benchmarks" <> "loading" <> FS.decodeString rndStr
  putStrLn $ "Working under the following temporary directory: " ++ FS.encodeString workingPath
  FS.removeTreeIfExists workingPath
  FS.createTree workingPath
  return workingPath


benchmarksGroup :: [BenchmarkInterface] -> FS.FilePath -> Int -> Benchmark
benchmarksGroup bis workingPath size =
  bgroup (show size)
  [ bgroup (benchName bi) $
      initializeBenchmarksGroup bi workingPath' size
    : if benchPersist bi then [openCloseBenchmarksGroup  bi workingPath' size] else []
  | bi <- bis
  ]
  where
    workingPath' = workingPath <> FS.decodeString (show size)


-- | The Initialize benchmarks measure how long it takes to open an
-- empty 'AcidState' component, call 'initialize' to populate it with
-- data, and optionally checkpoint before closing.
initializeBenchmarksGroup :: BenchmarkInterface -> FS.FilePath -> Int -> Benchmark
initializeBenchmarksGroup bi workingPath size =
  bgroup "Initialize"
  [ bench "Without checkpoint" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
        initializeClose bi workingPath size
  , bench "With checkpoint" $ perRunEnv (prepareInitialize bi workingPath) $ \ _ ->
        initializeCheckpointClose bi workingPath size
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
  putStrLn $ "Preparing instances for size " ++ show size

  let
    logsInstancePath = workingPath <> "logs-instance"
    checkpointInstancePath = workingPath <> "checkpoint-instance"

  FS.createTree logsInstancePath
  FS.createTree checkpointInstancePath

  putStrLn "Initializing"
  initialize bi checkpointInstancePath size $ \inst -> do

    putStrLn "Copying"
    FS.copy checkpointInstancePath logsInstancePath
    FS.removeFile $ logsInstancePath <> "open.lock"

    putStrLn "Checkpointing"
    Acid.createCheckpoint inst

    putStrLn "Closing"
    Acid.closeAcidState inst

    return (logsInstancePath, checkpointInstancePath)


initialize :: BenchmarkInterface -> FS.FilePath ->  Int -> (forall m . Acid.AcidState m -> IO r) -> IO r
initialize BenchmarkInterface{..} p size k = do
    inst <- benchOpen p
    let values = replicate size $ replicate 100 $ replicate 100 1
    mapM_ (benchUpdate inst) values
    k inst

initializeClose :: BenchmarkInterface -> FS.FilePath -> Int -> IO ()
initializeClose bi p size = initialize bi p size Acid.closeAcidState

initializeCheckpointClose :: BenchmarkInterface -> FS.FilePath -> Int -> IO ()
initializeCheckpointClose bi p size =
  initialize bi p size $ \ inst -> do
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
