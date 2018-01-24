{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module FileIO(FHandle,open,write,flush,close,obtainPrefixLock,releasePrefixLock,PrefixLock) where
import System.Posix(Fd(Fd),
                    openFd,
                    fdWriteBuf,
                    fdToHandle,
                    closeFd,
                    OpenMode(WriteOnly,ReadWrite),
                    exclusive, trunc,
                    defaultFileFlags,
                    stdFileMode
                   )
import Data.Word(Word8,Word32)
import Foreign(Ptr)
import Foreign.C(CInt(..))
import System.IO

import Data.Maybe (listToMaybe)
import qualified System.IO.Error as SE
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)
import Control.Exception.Extensible as E
import System.Directory         ( createDirectoryIfMissing, removeFile, canonicalizePath)
import System.FilePath

#if defined(USE_UNIX_SOCKET_AS_LOCK)
import Network.Socket hiding (close)
import qualified Network.Socket as Socket (close)
#else
#endif

#if defined(USE_UNIX_SOCKET_AS_LOCK)
newtype PrefixLock = PrefixLock Socket
#else
newtype PrefixLock = PrefixLock FilePath
#endif

data FHandle = FHandle Fd

-- should handle opening flags correctly
open :: FilePath -> IO FHandle
open filename = fmap FHandle $ openFd filename WriteOnly (Just stdFileMode) defaultFileFlags

write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle fd) data' length = fmap fromIntegral $ fdWriteBuf fd data' $ fromIntegral length

-- Handle error values?
flush :: FHandle -> IO ()
flush (FHandle (Fd c_fd)) = c_fsync c_fd >> return ()

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt

close :: FHandle -> IO ()
close (FHandle fd) = closeFd fd

-- Unix needs to use a special open call to open files for exclusive writing
--openExclusively :: FilePath -> IO Handle
--openExclusively fp =
--    fdToHandle =<< openFd fp ReadWrite (Just 0o600) flags
--    where flags = defaultFileFlags {exclusive = True, trunc = True}

#if defined(USE_UNIX_SOCKET_AS_LOCK)

obtainPrefixLock :: FilePath -> IO PrefixLock
obtainPrefixLock prefix = do
    sock <- socket AF_UNIX Datagram defaultProtocol
    -- Create a unix socket in the abstract namespace.
    -- This is non portable and works only on Linux, 
    -- but we have the guarantee that both close()
    -- and the process dying will release it, without
    -- a need for a manual unlink()
    -- The name of the socket must be unique, hence we take
    -- the full absolute path of the directory we want to lock
    absPrefix <- canonicalizePath prefix
    let fp = "\0" ++ absPrefix ++ ".lock"
    let handler :: IOException -> IO ()
        handler _ = throw (userError "Locked")
    catch (bind sock (SockAddrUnix fp)) handler
    return (PrefixLock sock)

-- |Reliquish the lock by closing the socket.
releasePrefixLock :: PrefixLock -> IO ()
releasePrefixLock (PrefixLock sock) =
    catch (Socket.close sock) handler
    where handler :: IOException -> IO ()
          handler e = return ()

#else

obtainPrefixLock :: FilePath -> IO PrefixLock
obtainPrefixLock prefix = do
    checkLock fp >> takeLock fp
    where fp = prefix ++ ".lock"

-- |Read the lock and break it if the process is dead.
checkLock :: FilePath -> IO ()
checkLock fp = readLock fp >>= maybeBreakLock fp

-- |Read the lock and return the process id if possible.
readLock :: FilePath -> IO (Maybe ProcessID)
readLock fp = try (readFile fp) >>=
              return . either (checkReadFileError fp) (fmap (fromInteger . read) . listToMaybe . lines)

-- |Is this a permission error?  If so we don't have permission to
-- remove the lock file, abort.
checkReadFileError :: [Char] -> IOError -> Maybe ProcessID
checkReadFileError fp e | SE.isPermissionError e = throw (userError ("Could not read lock file: " ++ show fp))
                        | SE.isDoesNotExistError e = Nothing
                        | True = throw e

maybeBreakLock :: FilePath -> Maybe ProcessID -> IO ()
maybeBreakLock fp Nothing =
    -- The lock file exists, but there's no PID in it.  At this point,
    -- we will break the lock, because the other process either died
    -- or will give up when it failed to read its pid back from this
    -- file.
    breakLock fp
maybeBreakLock fp (Just pid) = do
  -- The lock file exists and there is a PID in it.  We can break the
  -- lock if that process has died.
  -- getProcessStatus only works on the children of the calling process.
  -- exists <- try (getProcessStatus False True pid) >>= either checkException (return . isJust)
  exists <- doesProcessExist pid
  case exists of
    True -> throw (lockedBy fp pid)
    False -> breakLock fp

doesProcessExist :: ProcessID -> IO Bool
doesProcessExist pid =
    -- Implementation 1
    -- doesDirectoryExist ("/proc/" ++ show pid)
    -- Implementation 2
    try (signalProcess nullSignal pid) >>= return . either checkException (const True)
    where checkException e | SE.isDoesNotExistError e = False
                           | True = throw e

-- |We have determined the locking process is gone, try to remove the
-- lock.
breakLock :: FilePath -> IO ()
breakLock fp = try (removeFile fp) >>= either checkBreakError (const (return ()))

-- |An exception when we tried to break a lock, if it says the lock
-- file has already disappeared we are still good to go.
checkBreakError :: IOError -> IO ()
checkBreakError e | SE.isDoesNotExistError e = return ()
                  | True = throw e

-- |Try to create lock by opening the file with the O_EXCL flag and
-- writing our PID into it.  Verify by reading the pid back out and
-- matching, maybe some other process slipped in before we were done
-- and broke our lock.
takeLock :: FilePath -> IO PrefixLock
takeLock fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  h <- openFd fp ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True}) >>= fdToHandle
  pid <- getProcessID
  hPutStrLn h (show pid) >> hClose h
  -- Read back our own lock and make sure its still ours
  readLock fp >>= maybe (throw (cantLock fp pid))
                        (\ pid' -> if pid /= pid'
                                   then throw (stolenLock fp pid pid')
                                   else return (PrefixLock fp))

-- |An exception saying the data is locked by another process.
lockedBy :: (Show a) => FilePath -> a -> SomeException
lockedBy fp pid = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Locked by " ++ show pid) Nothing (Just fp))

-- |An exception saying we don't have permission to create lock.
cantLock :: FilePath -> ProcessID -> SomeException
cantLock fp pid = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Process " ++ show pid ++ " could not create a lock") Nothing (Just fp))

-- |An exception saying another process broke our lock before we
-- finished creating it.
stolenLock :: FilePath -> ProcessID -> ProcessID -> SomeException
stolenLock fp pid pid' = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Process " ++ show pid ++ "'s lock was stolen by process " ++ show pid') Nothing (Just fp))

-- |Relinquish the lock by removing it and then verifying the removal.
releasePrefixLock :: PrefixLock -> IO ()
releasePrefixLock (PrefixLock fp) =
    dropLock >>= either checkDrop return
    where
      dropLock = try (removeFile fp)
      checkDrop e | SE.isDoesNotExistError e = return ()
                  | True = throw e

#endif


 