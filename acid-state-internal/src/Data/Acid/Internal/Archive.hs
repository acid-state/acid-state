module Data.Acid.Internal.Archive
    ( Entry
    , Entries(..)
    , entriesToList
    , entriesToListNoFail

    , Archiver(..)
    ) where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Monoid

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
