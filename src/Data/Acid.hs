-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 AcidState container using a transaction log on disk.

 To see how it all fits together, have a look at these example
 <http://mirror.seize.it/acid-state/examples/>.

-}
 
module Data.Acid
    ( AcidState
    , openAcidState
    , openAcidStateFrom
    , closeAcidState
    , createCheckpoint
    , update
    , query
    , EventResult
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , IsAcidic
    , makeAcidic
    ) where

import Data.Acid.Local
import Data.Acid.TemplateHaskell
