{-# LANGUAGE CPP #-}

-- | A log is a stack of entries that supports efficient pushing of new entries
-- and fetching of old. It can be considered an extendible array of entries.
--
module Data.Acid.Log
    ( FileLog(..)
    , LogKey(..)
    , EntryId
    , openFileLog
    , closeFileLog
    , pushEntry
    , pushAction
    , ensureLeastEntryId
    , readEntriesFrom
    , rollbackTo
    , rollbackWhile
    , newestEntry
    , askCurrentEntryId
    , cutFileLog
    , archiveFileLog
    , findLogFiles
    ) where

import Data.Acid.Archive (Archiver(..), Entries(..), entriesToList)
import Data.Acid.Core
import System.Directory
import System.FilePath
import System.IO
import FileIO

import Foreign.Ptr
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Unsafe as Strict
import Data.List                                 ( (\\), stripPrefix, sort )
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid                               ( (<>) )
#endif
import Text.Printf                               ( printf )

import Paths_acid_state                          ( version )
import Data.Version                              ( showVersion )
import Control.Exception                         ( handle, IOException )

type EntryId = Int

data FileLog object
    = FileLog { logIdentifier  :: LogKey object
              , logCurrent     :: MVar FHandle -- Handle
              , logNextEntryId :: TVar EntryId
              , logQueue       :: TVar ([Lazy.ByteString], [IO ()])
              , logThreads     :: [ThreadId]
              }

data LogKey object
    = LogKey
      { logDirectory :: FilePath
      , logPrefix    :: String
      , logSerialiser :: Serialiser object
      , logArchiver   :: Archiver
      }

formatLogFile :: String -> EntryId -> String
formatLogFile = printf "%s-%010d.log"

findLogFiles :: LogKey object -> IO [(EntryId, FilePath)]
findLogFiles identifier = do
  createDirectoryIfMissing True (logDirectory identifier)
  files <- getDirectoryContents (logDirectory identifier)
  return  [ (tid, logDirectory identifier </> file)
          | file <- files
          , logFile <- maybeToList (stripPrefix (logPrefix identifier ++ "-") file)
          , (tid, ".log") <- reads logFile ]


saveVersionFile :: LogKey object -> IO ()
saveVersionFile key = do
  exist <- doesFileExist versionFile
  unless exist $ writeFile versionFile (showVersion version)
 where
  versionFile = logDirectory key </> logPrefix key <.> "version"

openFileLog :: LogKey object -> IO (FileLog object)
openFileLog identifier = do
  logFiles <- findLogFiles identifier
  saveVersionFile identifier
  currentState <- newEmptyMVar
  queue <- newTVarIO ([], [])
  nextEntryRef <- newTVarIO 0
  tid1 <- myThreadId
  tid2 <- forkIO $ fileWriter (logArchiver identifier) currentState queue tid1
  let fLog = FileLog { logIdentifier  = identifier
                     , logCurrent     = currentState
                     , logNextEntryId = nextEntryRef
                     , logQueue       = queue
                     , logThreads     = [tid2] }
  if null logFiles
     then do let currentEntryId = 0
             handle <- open (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId)
             putMVar currentState handle
     else do let (lastFileEntryId, lastFilePath) = maximum logFiles
             entries <- readEntities (logArchiver identifier) lastFilePath
             let currentEntryId = lastFileEntryId + length entries
             atomically $ writeTVar nextEntryRef currentEntryId
             handle <- open (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId)
             putMVar currentState handle
  return fLog

fileWriter :: Archiver -> MVar FHandle -> TVar ([Lazy.ByteString], [IO ()]) -> ThreadId -> IO ()
fileWriter archiver currentState queue parentTid = forever $ do
  (entries, actions) <- atomically $ do
    (entries, actions) <- readTVar queue
    when (null entries && null actions) retry
    writeTVar queue ([], [])
    return (reverse entries, reverse actions)
  handle (\e -> throwTo parentTid (e :: IOException)) $
    withMVar currentState $ \fd -> do
      let arch = archiveWrite archiver entries
      writeToDisk fd (repack arch)
  sequence_ actions
  yield

-- | Repack a lazy bytestring into larger blocks that can be efficiently written
-- to disk.
repack :: Lazy.ByteString -> [Strict.ByteString]
repack = worker
  where
    worker bs
      | Lazy.null bs = []
      | otherwise    = Strict.concat (Lazy.toChunks (Lazy.take blockSize bs)) : worker (Lazy.drop blockSize bs)
    blockSize = 4*1024

writeToDisk :: FHandle -> [Strict.ByteString] -> IO ()
writeToDisk _ [] = return ()
writeToDisk handle xs = do
  mapM_ worker xs
  flush handle
 where
  worker bs = do
    let len = Strict.length bs
    count <- Strict.unsafeUseAsCString bs $ \ptr -> write handle (castPtr ptr) (fromIntegral len)
    when (fromIntegral count < len) $
       worker (Strict.drop (fromIntegral count) bs)


closeFileLog :: FileLog object -> IO ()
closeFileLog fLog =
  modifyMVar_ (logCurrent fLog) $ \handle -> do
    close handle
    _ <- forkIO $ forM_ (logThreads fLog) killThread
    return $ error "Data.Acid.Log: FileLog has been closed"

readEntities :: Archiver -> FilePath -> IO [Lazy.ByteString]
readEntities archiver path = do
  archive <- Lazy.readFile path
  return $ entriesToList (archiveRead archiver archive)

ensureLeastEntryId :: FileLog object -> EntryId -> IO ()
ensureLeastEntryId fLog youngestEntry = do
  atomically $ do
    entryId <- readTVar (logNextEntryId fLog)
    writeTVar (logNextEntryId fLog) (max entryId youngestEntry)
  cutFileLog fLog
  return ()

-- | Read all durable entries younger than the given 'EntryId'. Note that
-- entries written during or after this call won't be included in the returned
-- list.
readEntriesFrom :: FileLog object -> EntryId -> IO [object]
readEntriesFrom fLog youngestEntry = do
  -- Cut the log so we can read written entries without interfering
  -- with the writing of new entries.
  entryCap <- cutFileLog fLog
  -- We're interested in these entries: youngestEntry <= x < entryCap.
  logFiles <- findLogFiles (logIdentifier fLog)
  let sorted = sort logFiles
      relevant = filterLogFiles (Just youngestEntry) (Just entryCap) sorted
      firstEntryId = case relevant of
                       []                     -> 0
                       ( logFile : _logFiles) -> rangeStart logFile
  -- XXX: Strict bytestrings are used due to a performance bug in
  -- cereal-0.3.5.2 and binary-0.7.1.0. The code should revert back
  -- to lazy bytestrings once the bug has been fixed.
  archive <- liftM Lazy.fromChunks $ mapM (Strict.readFile . snd) relevant
  let entries = entriesToList $ archiveRead (logArchiver identifier) archive
  return $ map (decode' identifier)
         $ take (entryCap - youngestEntry)             -- Take events under the eventCap.
         $ drop (youngestEntry - firstEntryId) entries -- Drop entries that are too young.
 where
  rangeStart (firstEntryId, _path) = firstEntryId
  identifier = logIdentifier fLog

-- | Obliterate log entries younger than or equal to the 'EntryId'. Very unsafe,
-- can't be undone
rollbackTo :: LogKey object -> EntryId -> IO ()
rollbackTo identifier youngestEntry = do
  logFiles <- findLogFiles identifier
  let sorted = sort logFiles
      loop [] = return ()
      loop ((rangeStart, path) : xs)
        | rangeStart >= youngestEntry = removeFile path >> loop xs
        | otherwise = do
            archive <- Strict.readFile path
            pathHandle <- openFile path WriteMode
            let entries = entriesToList $ archiveRead (logArchiver identifier) (Lazy.fromChunks [archive])
                entriesToKeep = take (youngestEntry - rangeStart + 1) entries
                lengthToKeep = Lazy.length (archiveWrite (logArchiver identifier) entriesToKeep)
            hSetFileSize pathHandle (fromIntegral lengthToKeep)
            hClose pathHandle
  loop (reverse sorted)

-- | Obliterate log entries as long as the filter function returns @True@.
rollbackWhile :: LogKey object
  -> (object -> Bool) -- ^ the filter function
  -> IO ()
rollbackWhile identifier filterFn = do
  logFiles <- findLogFiles identifier
  let sorted = sort logFiles
      loop [] = return ()
      loop ((_rangeStart, path) : xs) = do
        archive <- Strict.readFile path
        let entries = entriesToList $ archiveRead (logArchiver identifier) (Lazy.fromChunks [archive])
            entriesToSkip = takeWhile (filterFn . decode' identifier) $ reverse entries
            skip_size = Lazy.length (archiveWrite (logArchiver identifier) entriesToSkip)
            orig_size = fromIntegral $ Strict.length archive
            new_size = orig_size - skip_size
        if new_size == 0
           then do removeFile path; loop xs
           else do pathHandle <- openFile path WriteMode
                   hSetFileSize pathHandle (fromIntegral new_size)
                   hClose pathHandle
  loop (reverse sorted)

-- | Filter out log files that are outside the min_entry/max_entry range.
--
--     minEntryId <= x < maxEntryId
filterLogFiles
  :: Maybe EntryId
  -- ^ minEntryId
  -> Maybe EntryId
  -- ^ maxEntryId
  -> [(EntryId, FilePath)] -> [(EntryId, FilePath)]
filterLogFiles minEntryIdMb maxEntryIdMb logFiles = worker logFiles
  where
    worker [] = []
    worker [ logFile ]
      | ltMaxEntryId (rangeStart logFile) -- If the logfile starts before our maxEntryId then we're intersted.
      = [ logFile ]
      | otherwise
      = []
    worker ( left : right : xs)
      | ltMinEntryId (rangeStart right) -- If 'right' starts before our minEntryId then we can discard 'left'.
      = worker (right : xs)
      | ltMaxEntryId (rangeStart left)  -- If 'left' starts before our maxEntryId then we're interested.
      = left : worker (right : xs)
      | otherwise                       -- If 'left' starts after our maxEntryId then we're done.
      = []
    ltMinEntryId = case minEntryIdMb of Nothing         -> const False
                                        Just minEntryId -> (<= minEntryId)
    ltMaxEntryId = case maxEntryIdMb of Nothing         -> const True
                                        Just maxEntryId -> (< maxEntryId)
    rangeStart (firstEntryId, _path) = firstEntryId

-- | Move all log files that do not contain entries equal or higher than the
-- given entryId into an @Archive/@ directory.
archiveFileLog :: FileLog object -> EntryId -> IO ()
archiveFileLog fLog entryId = do
  logFiles <- findLogFiles (logIdentifier fLog)
  let sorted = sort logFiles
      relevant = filterLogFiles Nothing (Just entryId) sorted
              \\ filterLogFiles (Just entryId) (Just (entryId+1))  sorted

  createDirectoryIfMissing True archiveDir
  forM_ relevant $ \(_startEntry, logFilePath) ->
    renameFile logFilePath (archiveDir </> takeFileName logFilePath)
 where
  archiveDir = logDirectory (logIdentifier fLog) </> "Archive"

getNextDurableEntryId :: FileLog object -> IO EntryId
getNextDurableEntryId fLog  = atomically $ do
  (entries, _) <- readTVar (logQueue fLog)
  next <- readTVar (logNextEntryId fLog)
  return (next - length entries)

cutFileLog :: FileLog object -> IO EntryId
cutFileLog fLog = do
  mvar <- newEmptyMVar
  let action = do currentEntryId <- getNextDurableEntryId fLog
                  modifyMVar_ (logCurrent fLog) $ \old ->
                    do close old
                       open (logDirectory key </> formatLogFile (logPrefix key) currentEntryId)
                  putMVar mvar currentEntryId
  pushAction fLog action
  takeMVar mvar
 where
  key = logIdentifier fLog

-- | Finds the newest entry in the log. Doesn't work on open logs. Do not use
-- after the log has been opened.
--
-- Implementation:
--
-- - Search the newest log files first.
-- - Once a file containing at least one valid entry is found, return the last
--   entry in that file.
newestEntry :: LogKey object -> IO (Maybe object)
newestEntry identifier = do
  logFiles <- findLogFiles identifier
  let sorted = reverse $ sort logFiles
      (_eventIds, files) = unzip sorted
  worker files
 where
  worker [] = return Nothing
  worker (logFile:logFiles) = do
    -- XXX: Strict bytestrings are used due to a performance bug in
    -- cereal-0.3.5.2 and binary-0.7.1.0. The code should revert back
    -- to lazy bytestrings once the bug has been fixed.
    archive <- fmap Lazy.fromStrict $ Strict.readFile logFile
    case archiveRead (logArchiver identifier) archive of
      Done            -> worker logFiles
      Next entry next -> return $ Just (decode' identifier (lastEntry entry next))
      Fail msg        -> error $ "Data.Acid.Log: " <> msg
  lastEntry entry Done          = entry
  lastEntry entry (Fail msg)    = error $ "Data.Acid.Log: " <> msg
  lastEntry _ (Next entry next) = lastEntry entry next

-- | Schedule a new log entry. This call does not block. The given IO action
-- runs once the object is durable. The IO action blocks the serialization of
-- events so it should be swift.
pushEntry :: FileLog object -> object -> IO () -> IO ()
pushEntry fLog object finally = atomically $ do
  tid <- readTVar (logNextEntryId fLog)
  writeTVar (logNextEntryId fLog) $! tid+1
  (entries, actions) <- readTVar (logQueue fLog)
  writeTVar (logQueue fLog) ( encoded : entries, finally : actions )
 where
  encoded = Lazy.fromChunks [ Strict.copy $ Lazy.toStrict $
              serialiserEncode (logSerialiser (logIdentifier fLog)) object ]

-- | The given IO action is executed once all previous entries are durable.
pushAction :: FileLog object -> IO () -> IO ()
pushAction fLog finally = atomically $ do
  (entries, actions) <- readTVar (logQueue fLog)
  writeTVar (logQueue fLog) (entries, finally : actions)

askCurrentEntryId :: FileLog object -> IO EntryId
askCurrentEntryId fLog = atomically $
  readTVar (logNextEntryId fLog)


-- FIXME: Check for unused input.
decode' :: LogKey object -> Lazy.ByteString -> object
decode' s inp =
  case serialiserDecode (logSerialiser s) inp of
    Left msg  -> error $ "Data.Acid.Log: " <> msg
    Right val -> val
