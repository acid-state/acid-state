module Data.State.Acid.Log
    ( FileLog
    , LogKey(..)
    , EntryId
    , openFileLog
    , closeFileLog
    , pushEntry
    , newestEntry
    , entriesAfterCutoff
    , askCurrentEntryId
    ) where

import Data.State.Acid.Archive as Archive
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as Lazy
--import qualified Data.ByteString as Strict
import Data.List
import Data.Maybe
import Data.Binary

import Text.Printf                               ( printf )

import Paths_acid_state                          ( version )
import Data.Version                              ( showVersion )

type EntryId = Int

data FileLog object
    = FileLog { logIdentifier  :: LogKey object
              , logCurrent     :: MVar (Handle)
              , logNextEntryId :: TVar EntryId
              , logQueue       :: TVar [(Lazy.ByteString,IO ())]
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
         queue <- newTVarIO []
         nextEntryRef <- newTVarIO 0
         tid2 <- forkIO $ forever $ do pairs <- atomically $ do vals <- readTVar queue
                                                                guard (not $ null vals)
                                                                writeTVar queue []
                                                                return (reverse vals)
                                       let (entries, actions) = unzip pairs
                                       withMVar currentState $ \handle ->
                                         do let arch = Archive.packEntries entries
                                            seq (Lazy.length arch) (return ())
                                            Lazy.hPutStr handle arch
                                            hFlush handle
                                            return ()
                                       sequence_ actions
                                       yield
         let log = FileLog { logIdentifier  = identifier
                           , logCurrent     = currentState
                           , logNextEntryId = nextEntryRef
                           , logQueue       = queue
                           , logThreads     = [tid2] }
         if null logFiles
            then do let currentEntryId = 0
                    currentHandle <- openBinaryFile (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId) WriteMode
                    putMVar currentState currentHandle
            else do let (lastFileEntryId, lastFilePath) = maximum logFiles
                    entries <- readEntities lastFilePath
                    let currentEntryId = lastFileEntryId + length entries
                    atomically $ writeTVar nextEntryRef currentEntryId
                    currentHandle <- openFile (logDirectory identifier </> formatLogFile (logPrefix identifier) currentEntryId) WriteMode
                    putMVar currentState currentHandle
         return log

closeFileLog :: FileLog object -> IO ()
closeFileLog log
    = modifyMVar_ (logCurrent log) $ \handle ->
      do hClose handle
         forkIO $ forM_ (logThreads log) killThread
         return $ error "FileLog has been closed"

readEntities :: FilePath -> IO [Lazy.ByteString]
readEntities path
    = do archive <- Lazy.readFile path
         return $ worker (Archive.readEntries archive)
    where worker Done = []
          worker (Next entry next)
              = entry : worker next
          worker Fail{} = []

-- Return entries newer than or equal to the cutoff.
-- Do not use after the log has been opened.
-- Implementation: 1) find the files that /may/ contain entries
--                    younger than the cutoff.
--                 2) parse all the entries in those files.
--                 3) drop the entries that are too old.
entriesAfterCutoff :: Binary object => LogKey object -> EntryId -> IO [object]
entriesAfterCutoff identifier cutoff
    = do logFiles <- findLogFiles identifier
         let sorted   = reverse $ sort logFiles       -- newest files first
             relevant = reverse $ takeRelevant sorted -- oldest files first
             (entryIds, files) = unzip relevant
         case entryIds of
           [] -> return []
           (firstEntryId : _)
             -> do archive <- liftM Lazy.concat $ mapM Lazy.readFile files
                   let events = entriesToList $ readEntries archive
                   return $ map decode $ drop (cutoff - firstEntryId) events
    where takeRelevant [] = []
          takeRelevant ((firstEntryId, file) : rest)
              | firstEntryId < cutoff
              = [ (firstEntryId, file) ]
              | otherwise
              = (firstEntryId, file) : takeRelevant rest


-- Finds the newest entry in the log. Doesn't work on open logs.
-- Do not use after the log has been opened.
-- Implementation: Search the newest log files first. Once a file
--                 containing at least one valid entry is found,
--                 return the last entry in that file.
newestEntry :: Binary object => LogKey object -> IO (Maybe object)
newestEntry identifier
    = do logFiles <- findLogFiles identifier
         let sorted = reverse $ sort logFiles
             (eventIds, files) = unzip sorted
         archives <- mapM Lazy.readFile files
         return $ worker archives
    where worker [] = Nothing
          worker (archive:archives)
              = case Archive.readEntries archive of
                  Done            -> worker archives
                  Next entry next -> Just (decode (lastEntry entry next))
                  Fail{}          -> worker archives
          lastEntry entry Done   = entry
          lastEntry entry Fail{} = entry
          lastEntry _ (Next entry next) = lastEntry entry next

-- Schedule a new log entry. May not block.
pushEntry :: Binary object => FileLog object -> object -> IO () -> IO ()
pushEntry log object finally
    = atomically $
      do tid <- readTVar (logNextEntryId log)
         writeTVar (logNextEntryId log) (tid+1)
         pairs <- readTVar (logQueue log)
         writeTVar (logQueue log) ((encoded, finally) : pairs)
    where encoded = encode object

askCurrentEntryId :: FileLog object -> IO EntryId
askCurrentEntryId log
    = atomically $ readTVar (logNextEntryId log)
