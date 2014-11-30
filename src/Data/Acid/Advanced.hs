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
    , update_
    , update_'
    , query'
    , Method(..)
    , IsAcidic(..)
    , Event(..)
    ) where

import Data.Acid.Abstract
import Data.Acid.Core
import Data.Acid.Common

