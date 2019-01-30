{-# LANGUAGE DoAndIfThenElse #-}
{-
Format:
 |content length| crc16   | content |
 |8 bytes       | 2 bytes | n bytes |
-}
module Data.Acid.Archive
    ( Entry
    , Entries(..)
    , putEntries
    , packEntries
    , readEntries
    , entriesToList
    , entriesToListNoFail
    , Archiver(..)
    , defaultArchiver
    ) where

import           Data.Acid.CRC

import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Lazy   as Lazy
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Serialize.Get     hiding (Result (..))
import qualified Data.Serialize.Get     as Serialize

-- | A bytestring that represents an entry in an archive.
type Entry = Lazy.ByteString

-- | Result of unpacking an archive.  This is essentially a list of
-- 'Entry', but may terminate in 'Fail' if the archive format is
-- incorrect.
data Entries = Done | Next Entry Entries | Fail String
    deriving (Show)

-- | Convert 'Entries' to a normal list, calling 'error' if there was
-- a failure in unpacking the archive.
entriesToList :: Entries -> [Entry]
entriesToList Done              = []
entriesToList (Next entry next) = entry : entriesToList next
entriesToList (Fail msg)        = error $ "Data.Acid.Archive: " <> msg

-- | Convert 'Entries' to a normal list, silently ignoring a failure
-- to unpack the archive and instead returning a truncated list.
entriesToListNoFail :: Entries -> [Entry]
entriesToListNoFail Done              = []
entriesToListNoFail (Next entry next) = entry : entriesToListNoFail next
entriesToListNoFail Fail{}            = []


-- | Interface for the lowest level of the serialisation layer, which
-- handles packing lists of 'Entry' elements (essentially just
-- bytestrings) into a single bytestring, perhaps with error-checking.
--
-- Any @'Archiver'{'archiveWrite', 'archiveRead'}@ must satisfy the
-- round-trip property:
--
-- > forall xs . entriesToList (archiveRead (archiveWrite xs)) == xs
--
-- Moreover, 'archiveWrite' must be a monoid homomorphism, so that
-- concatenating archives is equivalent to concatenating the lists of
-- entries that they represent:
--
-- > archiveWrite [] == empty
-- > forall xs ys . archiveWrite xs <> archiveWrite ys == archiveWrite (xs ++ ys)
data Archiver
    = Archiver
      { archiveWrite :: [Entry] -> Lazy.ByteString
        -- ^ Pack a list of entries into a bytestring.

      , archiveRead  :: Lazy.ByteString -> Entries
        -- ^ Unpack a bytestring as a list of 'Entries', including the
        -- possibility of failure if the format is invalid.
      }

-- | Standard (and historically the only) implementation of the
-- 'Archiver' interface.  This represents each entry in the following
-- format:
--
-- > | entry length | crc16   | entry   |
-- > | 8 bytes      | 2 bytes | n bytes |
defaultArchiver :: Archiver
defaultArchiver = Archiver packEntries readEntries


putEntry :: Entry -> Builder
putEntry content
    = word64LE contentLength !<>
      word16LE contentHash !<>
      lazyByteString content
    where contentLength = fromIntegral $ Lazy.length content
          contentHash   = crc16 content
          a !<> b = let c = a <> b in c `seq` c

putEntries :: [Entry] -> Builder
putEntries = mconcat . map putEntry

packEntries :: [Entry] -> Lazy.ByteString
packEntries = toLazyByteString . putEntries

readEntries :: Lazy.ByteString -> Entries
readEntries bs
    = worker (Lazy.toChunks bs)
    where worker [] = Done
          worker (x:xs)
              = check (runGetPartial readEntry x) xs
          check result more
              = case result of
                  Serialize.Done entry rest
                      | Strict.null rest    -> Next entry (worker more)
                      | otherwise           -> Next entry (worker (rest:more))
                  Serialize.Fail msg _      -> Fail msg
                  Serialize.Partial cont    -> case more of
                                                 []     -> check (cont Strict.empty) []
                                                 (x:xs) -> check (cont x) xs

readEntry :: Get Entry
readEntry
    = do contentLength <- getWord64le
         contentChecksum <-getWord16le
         content <- getLazyByteString_fast (fromIntegral contentLength)
         if crc16 content /= contentChecksum
           then fail "Invalid hash"
           else return content

-- | Read a lazy bytestring WITHOUT any copying or concatenation.
getLazyByteString_fast :: Int -> Get Lazy.ByteString
getLazyByteString_fast = worker 0 []
  where
    worker counter acc n = do
      rem <- remaining
      if n > rem then do
         chunk <- getBytes rem
         _ <- ensure 1
         worker (counter + rem) (chunk:acc) (n-rem)
      else do
         chunk <- getBytes n
         return $ Lazy.fromChunks (reverse $ chunk:acc)










