{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module ParameterisedState (main) where

import Control.Monad.State.Strict

import Data.Acid
import qualified Data.Map as Map
import Data.SafeCopy (SafeCopy(..), deriveSafeCopy, base)
import Data.Serialize
import Data.Typeable
import GHC.Generics

data Entry k = Entry
    { key :: !k
    , val :: !Int
    }
    deriving (Eq, Ord, Generic)

instance Serialize k => Serialize (Entry k)

$(deriveSafeCopy 0 'base ''Entry)

newtype Store k = Store { store :: Map.Map k (Entry k) }
    deriving (Eq, Generic)

#if __GLASGOW_HASKELL__ <= 708
deriving instance Typeable1 Store
#endif

instance (Ord k, Serialize k, SafeCopy k, Typeable k) => SafeCopy (Store k)
instance (Ord k, Serialize k) => Serialize (Store k)

insertStore
    :: (Ord k, Serialize k)
    => Entry k
    -> Update (Store k) k
insertStore item = do
    modify $ \(Store s) -> Store $ Map.insert (key item) item s
    return (key item)

makeAcidic ''Store [ 'insertStore ]


main :: IO ()
main = do st <- openLocalState (Store Map.empty :: Store String)
          k  <- update st (InsertStore (Entry "A" 42))
          putStrLn k
