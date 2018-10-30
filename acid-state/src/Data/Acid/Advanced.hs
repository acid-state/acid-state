-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid.Advanced
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 Home of the more specialized functions.

-}
module Data.Acid.Advanced
    ( scheduleUpdate
    , groupUpdates
    , update'
    , query'
    , Method(..)
    , IsAcidic(..)
    , Event(..)

    , safeCopySerialiser
    , safeCopyMethodSerialiser
    , defaultArchiver
    ) where

import Data.Acid.Abstract
import Data.Acid.Archive
import Data.Acid.Core
import Data.Acid.Common

