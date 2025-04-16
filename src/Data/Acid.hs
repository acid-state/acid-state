-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 AcidState container using a transaction log on disk.

 To see how it all fits together, have a look at these example
 <https://github.com/acid-state/acid-state/tree/master/examples>.

-}

module Data.Acid
    ( AcidState
    , openLocalState
    , openLocalStateFrom
    , closeAcidState
    , createCheckpoint
    , createCheckpointAndClose
    , createArchive
    , update
    , query
    , EventResult
    , EventState
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , IsAcidic
    , makeAcidic
    , liftQuery
    ) where

import Data.Acid.Local
import Data.Acid.Common
import Data.Acid.Abstract
import Data.Acid.TemplateHaskell
