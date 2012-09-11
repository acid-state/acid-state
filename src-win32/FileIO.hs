module FileIO(FHandle,open,write,flush,close,obtainPrefixLock,releasePrefixLock,PrefixLock) where
import System.Win32(HANDLE,
                    createFile,
                    gENERIC_WRITE,
                    fILE_SHARE_NONE,
                    cREATE_ALWAYS,
                    fILE_ATTRIBUTE_NORMAL,
                    win32_WriteFile,
                    flushFileBuffers,
                    closeHandle)
import Data.Word(Word8,Word32)
import Foreign(Ptr)
import System.IO
import System.Directory(createDirectoryIfMissing,removeFile)
import Control.Exception.Extensible(try,throw)
import Control.Exception(SomeException)

type PrefixLock = (FilePath, Handle)

data FHandle = FHandle HANDLE

open :: FilePath -> IO FHandle
open filename =
    fmap FHandle $ createFile filename gENERIC_WRITE fILE_SHARE_NONE Nothing cREATE_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing

write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle handle) data' length = win32_WriteFile handle data' length Nothing

flush :: FHandle -> IO ()
flush (FHandle handle) = flushFileBuffers handle

close :: FHandle -> IO ()
close (FHandle handle) = closeHandle handle

-- Windows opens files for exclusive writing by default
openExclusively :: FilePath -> IO Handle
openExclusively fp = openFile fp ReadWriteMode

obtainPrefixLock :: FilePath -> IO PrefixLock
obtainPrefixLock prefix = do
    createDirectoryIfMissing True prefix
    -- catchIO obtainLock onError
    catchIO obtainLock onError
    where fp = prefix ++ ".lock"
          obtainLock = do
              h <- openExclusively fp
              return (fp, h)
          onError e = do
              putStrLn "There may already be an instance of this application running, which could result in a loss of data."
              putStrLn ("Please make sure there is no other application attempting to access '" ++ prefix ++ "'")
              throw e

releasePrefixLock :: PrefixLock -> IO ()
releasePrefixLock (fp, h) = do
     tryE $ hClose h
     tryE $ removeFile fp
     return ()
