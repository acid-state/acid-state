{-# LANGUAGE ForeignFunctionInterface #-}
-- A log is a stack of entries that supports efficient pushing of
-- new entries and fetching of old. It can be considered an
-- extendible array of entries.
--
module Data.Acid.Log
    ( FileLog
    , LogKey(..)
    , EntryId
    , openFileLog
    , closeFileLog
    , pushEntry
    , pushAction
    , readEntriesFrom
    , newestEntry
    , askCurrentEntryId
    , cutFileLog
    , archiveFileLog
    ) where

import Data.Acid.Archive as Archive
import System.Directory
import System.FilePath
import System.IO
import System.Posix                              ( handleToFd, Fd(..), fdWriteBuf
                                                 , closeFd )
import Foreign.C
import Foreign.Ptr
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Unsafe as Strict
import Data.List
import Data.Maybe
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import Data.SafeCopy                             ( safePut, safeGet, SafeCopy )

import Text.Printf                               ( printf )

import Paths_acid_state                          ( version )
import Data.Version                              ( showVersion )

type EntryId = Int

data FileLog object
    = FileLog { logIdentifier  :: LogKey object
              , logCurrent     :: MVar Fd -- Handle
              , logNextEntryId :: TVar EntryId
              , logQueue       :: TVar ([Lazy.ByteString], [IO ()])
              , logThreads     :: [ThreadId]
              }

data LogKey object
    = LogKey
      { logDirectory :: FilePath
      , logPrefix    :: String
      }

formatLogFile :: String -> EntryId -> String
formatLogFile tag n
    = printf "%s-%010d.log" tag n

findLogFiles :: LogKey object -> IO [(EntryId, FilePath)]
findLogFiles identifier
    = do createDirectoryIfMissing True (logDirectory identifier)
         files <- getDirectoryContents (logDirectory identifier)
         return  [ (tid, logDirectory identifier </> file)
                 | file <- files
                 , logFile <- maybeToList (stripPrefix (logPrefix identifier ++ "-") file)
                 , (tid, ".log") <- reads logFile ]


saveVersionFile :: LogKey object -> IO ()
saveVersionFile key
    = do exist <- doesFileExist versionFile
         unless exist $ writeFile versionFile (showVersion version)
    where versionFile = logDirectory key </> logPrefix key <.> "version"

openFileLog :: LogKey object -> IO (FileLog object)
openFileLog identifier
    = do logFiles <- findLogFiles identifier
         saveVersionFile identifier
         currentState <- newEmptyMVar
         queue <- newTVarIO ([], [])
         nextEntryRef <- newTVarIO 0
         tid2 <- forkIO $ fileWriter currentState queue
         let fLog = FileLog { logIdentifier  = identifier
                            , logCurrent     = currentState
                            , logNextEntryId = nextEntryRef
                            , logQueue       = queue
                            , logThreads     = [tid2] }
         if null logFiles
            then do let currentEntryId = 0
                    currentHandle <- openBinaryFile (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId) WriteMode
                    fd <- handleToFd currentHandle
                    putMVar currentState fd
            else do let (lastFileEntryId, lastFilePath) = maximum logFiles
                    entries <- readEntities lastFilePath
                    let currentEntryId = lastFileEntryId + length entries
                    atomically $ writeTVar nextEntryRef currentEntryId
                    currentHandle <- openBinaryFile (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId) WriteMode
                    fd <- handleToFd currentHandle
                    putMVar currentState fd
         return fLog

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt

fileWriter :: MVar Fd -> TVar ([Lazy.ByteString], [IO ()]) -> IO ()
fileWriter currentState queue
    = forever $
      do (entries, actions) <- atomically $ do (entries, actions) <- readTVar queue
                                               when (null entries && null actions) retry
                                               writeTVar queue ([], [])
                                               -- We don't actually have to reverse the actions
                                               -- but I don't think it hurts performance much.
                                               return (reverse entries, reverse actions)
         withMVar currentState $ \fd ->
           do let arch = Archive.packEntries entries
              writeToDisk fd (repack arch)
         sequence_ actions
         yield

-- Repack a lazy bytestring into larger blocks that can be efficiently written to disk.
repack :: Lazy.ByteString -> [Strict.ByteString]
repack = worker
    where worker bs
              | Lazy.null bs = []
              | otherwise    = Strict.concat (Lazy.toChunks (Lazy.take blockSize bs)) : worker (Lazy.drop blockSize bs)
          blockSize = 4*1024

writeToDisk :: Fd -> [Strict.ByteString] -> IO ()
writeToDisk _ [] = return ()
writeToDisk fd@(Fd c_fd) xs
    = do mapM_ worker xs
         c_fsync c_fd
         return ()
    where worker bs
              = do let len = Strict.length bs
                   count <- Strict.unsafeUseAsCString bs $ \ptr -> fdWriteBuf fd (castPtr ptr) (fromIntegral len)
                   if fromIntegral count < len
                      then worker (Strict.drop (fromIntegral count) bs)
                      else return ()


closeFileLog :: FileLog object -> IO ()
closeFileLog fLog
    = modifyMVar_ (logCurrent fLog) $ \fd ->
      do closeFd fd
         _ <- forkIO $ forM_ (logThreads fLog) killThread
         return $ error "FileLog has been closed"

readEntities :: FilePath -> IO [Lazy.ByteString]
readEntities path
    = do archive <- Lazy.readFile path
         return $ worker (Archive.readEntries archive)
    where worker Done = []
          worker (Next entry next)
              = entry : worker next
          worker Fail{} = []

-- Read all durable entries younger than the given EntryId.
-- Note that entries written during or after this call won't
-- be included in the returned list.
readEntriesFrom :: SafeCopy object => FileLog object -> EntryId -> IO [object]
readEntriesFrom fLog youngestEntry
    = do -- Cut the log so we can read written entries without interfering
         -- with the writing of new entries.
         entryCap <- cutFileLog fLog
         -- We're interested in these entries: youngestEntry <= x < entryCap.
         logFiles <- findLogFiles (logIdentifier fLog)
         let sorted = sort logFiles
             relevant = filterLogFiles (Just youngestEntry) (Just entryCap) sorted
             firstEntryId = case relevant of
                              []                     -> 0
                              ( logFile : _logFiles) -> rangeStart logFile

         archive <- liftM Lazy.concat $ mapM (Lazy.readFile . snd) relevant
         let entries = entriesToList $ readEntries archive
         return $ map decode'
                $ take (entryCap - youngestEntry)             -- Take events under the eventCap.
                $ drop (youngestEntry - firstEntryId) entries -- Drop entries that are too young.

    where rangeStart (firstEntryId, _path) = firstEntryId

-- Filter out log files that are outside the min_entry/max_entry range.
-- minEntryId <= x < maxEntryId
filterLogFiles :: Maybe EntryId -> Maybe EntryId -> [(EntryId, FilePath)] -> [(EntryId, FilePath)]
filterLogFiles minEntryIdMb maxEntryIdMb logFiles
  = worker logFiles
  where worker [] = []
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
                                            Just minEntryId -> \entryId -> entryId < minEntryId
        ltMaxEntryId = case maxEntryIdMb of Nothing         -> const True
                                            Just maxEntryId -> \entryId -> entryId < maxEntryId
        rangeStart (firstEntryId, _path) = firstEntryId

-- Move all log files that do not contain entries equal or higher than the given entryId
-- into an Archive/ directory.
archiveFileLog :: FileLog object -> EntryId -> IO ()
archiveFileLog fLog entryId
  = do logFiles <- findLogFiles (logIdentifier fLog)
       let sorted = sort logFiles
           relevant = filterLogFiles Nothing (Just entryId) sorted \\
                      filterLogFiles (Just (entryId+1)) (Just entryId) sorted

       createDirectoryIfMissing True archiveDir
       forM_ relevant $ \(_startEntry, logFilePath) ->
         renameFile logFilePath (archiveDir </> takeFileName logFilePath)
  where archiveDir = logDirectory (logIdentifier fLog) </> "Archive"

getNextDurableEntryId :: FileLog object -> IO EntryId
getNextDurableEntryId fLog
  = atomically $
    do (entries, _) <- readTVar (logQueue fLog)
       next <- readTVar (logNextEntryId fLog)
       return (next - length entries)

cutFileLog :: FileLog object -> IO EntryId
cutFileLog fLog
    = do mvar <- newEmptyMVar
         let action = do currentEntryId <- getNextDurableEntryId fLog
                         modifyMVar_ (logCurrent fLog) $ \old ->
                           do closeFd old
                              handleToFd =<< openBinaryFile (logDirectory key </> formatLogFile (logPrefix key) currentEntryId) WriteMode
                         putMVar mvar currentEntryId
         pushAction fLog action
         takeMVar mvar
    where key = logIdentifier fLog

-- Finds the newest entry in the log. Doesn't work on open logs.
-- Do not use after the log has been opened.
-- Implementation: Search the newest log files first. Once a file
--                 containing at least one valid entry is found,
--                 return the last entry in that file.
newestEntry :: SafeCopy object => LogKey object -> IO (Maybe object)
newestEntry identifier
    = do logFiles <- findLogFiles identifier
         let sorted = reverse $ sort logFiles
             (_eventIds, files) = unzip sorted
         archives <- mapM Lazy.readFile files
         return $ worker archives
    where worker [] = Nothing
          worker (archive:archives)
              = case Archive.readEntries archive of
                  Done            -> worker archives
                  Next entry next -> Just (decode' (lastEntry entry next))
                  Fail{}          -> worker archives
          lastEntry entry Done   = entry
          lastEntry entry Fail{} = entry
          lastEntry _ (Next entry next) = lastEntry entry next

-- Schedule a new log entry. This call does not block
-- The given IO action runs once the object is durable. The IO action
-- blocks the serialization of events so it should be swift.
pushEntry :: SafeCopy object => FileLog object -> object -> IO () -> IO ()
pushEntry fLog object finally
    = atomically $
      do tid <- readTVar (logNextEntryId fLog)
         writeTVar (logNextEntryId fLog) (tid+1)
         (entries, actions) <- readTVar (logQueue fLog)
         writeTVar (logQueue fLog) ( encoded : entries, finally : actions )
    where encoded = Lazy.fromChunks [ Strict.copy $ Put.runPut (safePut object) ]

-- The given IO action is executed once all previous entries are durable.
pushAction :: FileLog object -> IO () -> IO ()
pushAction fLog finally
    = atomically $
      do (entries, actions) <- readTVar (logQueue fLog)
         writeTVar (logQueue fLog) (entries, finally : actions)

askCurrentEntryId :: FileLog object -> IO EntryId
askCurrentEntryId fLog
    = atomically $ readTVar (logNextEntryId fLog)


-- FIXME: Check for unused input.
decode' :: SafeCopy object => Lazy.ByteString -> object
decode' inp
    = case Get.runGetLazy safeGet inp of
        Left msg  -> error msg
        Right val -> val
