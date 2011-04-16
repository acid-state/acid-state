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
    , readEntriesFrom
    , newestEntry
    , askCurrentEntryId
    ) where

import Data.Acid.Archive as Archive
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

fileWriter currentState queue
    = forever $
      do (entries, actions) <- atomically $ do (entries, actions) <- readTVar queue
                                               when (null entries && null actions) retry
                                               writeTVar queue ([], [])
                                               -- We don't actually have to reverse the actions
                                               -- but I don't think it hurts performance much.
                                               return (reverse entries, reverse actions)
         withMVar currentState $ \handle ->
           do let arch = Archive.packEntries entries
              Lazy.hPutStr handle arch
              hFlush handle
              return ()
         sequence_ actions
         yield


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

-- Read all durable entries younger than the given EntryId.
-- Note that entries written during or after this call won't
-- be included in the returned list.
readEntriesFrom :: Binary object => FileLog object -> EntryId -> IO [object]
readEntriesFrom log youngestEntry
    = do -- Cut the log so we can read written entries without interfering
         -- with the writing of new entries.
         entryCap <- cutFileLog log
         -- We're interested in these entries: youngestEntry <= x < entryCap.
         logFiles <- findLogFiles (logIdentifier log)
         let sorted = sort logFiles
             findRelevant [] = []
             findRelevant [ logFile ]
                 | youngestEntry <= rangeStart logFile && rangeStart logFile < entryCap
                 = [ logFile ]
                 | otherwise
                 = []
             findRelevant ( left : right : xs )
                 | youngestEntry >= rangeStart right -- All entries in 'path' must be too old if this is true
                 = findRelevant (right : xs)
                 | rangeStart left >= entryCap -- All files from now on contain entries that are too young.
                 = []
                 | otherwise
                 = left : findRelevant (right : xs)

             relevant = findRelevant sorted
             firstEntryId = case relevant of
                              []                     -> 0
                              ( logFile : _logFiles) -> rangeStart logFile

         archive <- liftM Lazy.concat $ mapM Lazy.readFile (map snd relevant)
         let entries = entriesToList $ readEntries archive
         return $ map decode
                $ take (entryCap - youngestEntry)             -- Take events under the eventCap.
                $ drop (youngestEntry - firstEntryId) entries -- Drop entries that are too young.

    where rangeStart (firstEntryId, _path) = firstEntryId


cutFileLog :: FileLog object -> IO EntryId
cutFileLog log
    = do mvar <- newEmptyMVar
         let action = do currentEntryId <- atomically $
                                           do (entries, _) <- readTVar (logQueue log)
                                              next <- readTVar (logNextEntryId log)
                                              return (next - length entries)
                         modifyMVar_ (logCurrent log) $ \old ->
                           do hClose old
                              openFile (logDirectory key </> formatLogFile (logPrefix key) currentEntryId) WriteMode
                         putMVar mvar currentEntryId
         atomically $
           do (entries, actions) <- readTVar (logQueue log)
              writeTVar (logQueue log) (entries, action : actions)
         takeMVar mvar
    where key = logIdentifier log

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

-- Schedule a new log entry. This call does not block
-- The given IO action runs once the object is durable. The IO action
-- blocks the serialization of events so it should be swift.
pushEntry :: Binary object => FileLog object -> object -> IO () -> IO ()
pushEntry log object finally
    = atomically $
      do tid <- readTVar (logNextEntryId log)
         writeTVar (logNextEntryId log) (tid+1)
         (entries, actions) <- readTVar (logQueue log)
         writeTVar (logQueue log) ( encoded : entries, finally : actions )
    where encoded = encode object

askCurrentEntryId :: FileLog object -> IO EntryId
askCurrentEntryId log
    = atomically $ readTVar (logNextEntryId log)
