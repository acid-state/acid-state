module Benchmark.Interface
  ( StateValue
  , mkStateValue

  , BenchmarkInterface(..)
  , memoryBenchmarkInterface
  , localBenchmarkInterface
  , defaultBenchmarkInterfaces
  ) where

import Benchmark.Prelude
import qualified Data.Acid as Acid
import qualified Data.Acid.Memory as Memory
import qualified Benchmark.FileSystem as FS
import qualified Benchmark.Model as Model


-- | The value stored in the state, currently always a list of lists of
-- integers.
type StateValue = [[Int]]

-- | Function to create a value to be stored.  This should typically be called
-- from within 'env' rather than a top-level CAF, to avoid keeping the result
-- around once it is evaluated and potentially interfering with other
-- benchmarks.
mkStateValue :: Int -> Int -> StateValue
mkStateValue size1 size2 = replicate size1 $ replicate size2 42


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
        , benchUpdate  :: Acid.AcidState m -> StateValue -> IO ()
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
