{-# LANGUAGE LambdaCase #-}

module Data.Acid.Repair
  ( repairFile
  , repairEvents
  , repairCheckpoints
  ) where

import qualified Data.Acid.Archive as Archive
import Data.Acid.Local (mkEventsLogKey, mkCheckpointsLogKey)
import Data.Acid.Log (LogKey)
import qualified Data.Acid.Log as Log
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort)
import System.Directory
import System.FilePath.Posix
import System.IO (hClose, openTempFile)

repairEntries :: Lazy.ByteString -> Lazy.ByteString
repairEntries =
  Archive.packEntries . Archive.entriesToListNoFail . Archive.readEntries

-- | @'repairFile' path@ will truncate the entries in @file@ until there are
-- only valid entries (if a corrupted entry is found, then the rest of the file
-- is truncated).
--
-- The old file will be copied to @path.bak@ (or @path.bak.1@, etc… if the file
-- already exists).
--
-- 'repairFile' tries very hard to avoid leaving files in an inconsistent state:
-- the truncated file is written in a temporary file, which is then moved into
-- place, similarly copies are performed with moves instead. Still this is not
-- fully atomic: there are two consecutive moves, so 'repairFile' may, in case
-- of crash, yield a state where the @path.bak@ file is there but no @path@ is
-- there anymore, this would require manual intervention.
repairFile :: FilePath -> IO ()
repairFile fp = do
    broken <- Lazy.readFile fp
    let repaired = repairEntries broken
    (tmp, temph) <- openTempFile (takeDirectory fp) (takeFileName fp)
      -- We use `openTempFile`, here, rather than `findNext` because we want to
      -- make extra-sure that we are not overriding an important file.
    hClose temph
      -- Closing immediately to benefit from the bracket guarantees of
      -- `writeFile`. A more elegant solution would be to use a `withTempFile`
      -- function, such as that from package `temporary`.
    Lazy.writeFile tmp repaired
    dropFile fp
    renameFile tmp fp

-- Repairs the files corresponding to the given 'LogKey'. It implements the
-- logic described in 'repairEvents'.
repairLogs :: LogKey object -> IO ()
repairLogs identifier = do
    logFiles <- Log.findLogFiles identifier
    let sorted = sort logFiles
        (_eventIds, files) = unzip sorted
    broken_files <- mapM needsRepair files
      -- We're doing a second deserialisation of the files here (see
      -- 'needsRepair'). It would be better, computation-time-wise to make as
      -- single pass and let `repairEntries`, for instance, return whether a fix
      -- is needed. But it's a lot of complication and requires loading the
      -- entire base in memory, rather than streaming files one-by-one. So it's
      -- better to just do the second pass.
    repair $ map snd $ dropWhile (\(b,_) -> not b) $ zip broken_files files
  where
    repair [] = return ()
    repair (file:rest) = do
      mapM_ dropFile (reverse rest)
      repairFile file

-- Moves (atomically) a file `path` to `path.bak` (or `path.bak.1`, etc… if the
-- file already exists).
dropFile :: FilePath -> IO ()
dropFile fp = do
    bak <- findNext (fp ++ ".bak")
      -- We're using `findNext` rather than `openTempFile`, here, because we
      -- want predictable names
    renameFile fp bak

-- | Repairs the WAL files with the following strategy:
--
-- * Let `f` be the oldest corrupted file.
-- * All files older than `f` is left untouched
-- * `f` is repaired with `repairFile`
-- * Old files younger than `f` is dropped (and saved to `path.bak`, or
--   `path.bak.1`, etc…)
--
-- In other words, all the log entries after the first corrupted entry is
-- dropped. The reasoning is that newer entries are likely not to make sense
-- after some entries have been removed from the log. This strategy guarantees a
-- consistent state, albeit a potentially old one.
repairEvents
  :: FilePath -- ^ Directory in which the events files can be found.
  -> IO ()
repairEvents directory =
    repairLogs (mkEventsLogKey directory noserialisation)
  where
    noserialisation =
      error "Repair.repairEvents: the serialisation layer shouldn't be forced"

-- | Repairs the checkpoints file using the following strategy:
--
-- * Every checkpoints file is repaired with `repairFile`
--
-- Checkpoints are mostly independent. Contrary to 'repairEvents', dropping a
-- checkpoint doesn't affect the consistency of later checkpoints.
repairCheckpoints
  :: FilePath -- ^ Directory in which the checkpoints files can be found.
  -> IO ()
repairCheckpoints directory = do
    let checkpointLogKey = mkCheckpointsLogKey directory noserialisation
    checkpointFiles <- Log.findLogFiles checkpointLogKey
    let (_eventIds, files) = unzip checkpointFiles
    mapM_ repairFile files
  where
    noserialisation =
      error "Repair.repairCheckpoints: the serialisation layer shouldn't be forced"

needsRepair :: FilePath -> IO Bool
needsRepair fp = do
    contents <- Lazy.readFile fp
    let entries = Archive.readEntries contents
    return $ entriesNeedRepair entries
  where
    entriesNeedRepair Archive.Fail{} = True
    entriesNeedRepair Archive.Done = False
    entriesNeedRepair (Archive.Next _ rest) = entriesNeedRepair rest

findNext :: FilePath -> IO (FilePath)
findNext fp = go 0
  where
    go n =
      let next = fileWithSuffix fp n in
      doesFileExist next >>= \case
        False -> return next
        True -> go (n+1)

fileWithSuffix :: FilePath -> Int -> FilePath
fileWithSuffix fp i =
  if i == 0 then fp
  else fp ++ "." ++ show i
