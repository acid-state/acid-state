{-
Format:
 |content length| crc16   | content |
 |8 bytes       | 2 bytes | n bytes |
-}
module Data.State.Acid.Archive
    ( Entry
    , Entries(..)
    , putEntries
    , packEntries
    , readEntries
    , entriesToList
    , entriesToListNoFail
    ) where

import Data.State.Acid.CRC

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Binary.Get
import Data.Binary.Builder
import Data.Monoid

type Entry = Lazy.ByteString
data Entries = Done | Next Entry Entries | Fail String
    deriving (Show)

entriesToList :: Entries -> [Entry]
entriesToList Done              = []
entriesToList (Next entry next) = entry : entriesToList next
entriesToList (Fail msg)        = fail msg

entriesToListNoFail :: Entries -> [Entry]
entriesToListNoFail Done              = []
entriesToListNoFail (Next entry next) = entry : entriesToListNoFail next
entriesToListNoFail Fail{}            = []

putEntry :: Entry -> Builder
putEntry content
    = putWord64le contentLength `mappend`
      putWord16le contentHash `mappend`
      fromLazyByteString content
    where contentLength = fromIntegral $ Lazy.length content
          contentHash   = crc16 content

putEntries :: [Entry] -> Builder
putEntries = mconcat . map putEntry

packEntries :: [Entry] -> Lazy.ByteString
packEntries = toLazyByteString . putEntries

readEntries :: Lazy.ByteString -> Entries
readEntries bs
    | Lazy.null bs
    = Done
    | Lazy.length header < headerSize
    = Fail "Incomplete header."
    | Lazy.length content /= fromIntegral contentLength
    = Fail "Insuficient content."
    | crc16 content /= contentHash
    = Fail "Invalid hash"
    | otherwise
    = Next content (readEntries rest)
    where header        = Lazy.take headerSize bs
          headerSize    = 10
          contentLength = fromIntegral $ runGet getWord64le header
          contentHash   = runGet getWord16le $ Lazy.drop 8 header
          content       = Lazy.take contentLength $ Lazy.drop headerSize bs
          rest          = Lazy.drop (contentLength+headerSize) bs

lazyToStrict :: Lazy.ByteString -> Strict.ByteString
lazyToStrict = Strict.concat . Lazy.toChunks

