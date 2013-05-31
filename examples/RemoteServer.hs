{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Control.Exception     ( bracket )
import Data.Acid             ( closeAcidState, openLocalState )
import Data.Acid.Remote      ( acidServer, sharedSecretCheck )
import Data.ByteString.Char8 ( pack )
import Data.Set              ( singleton )
import Network               ( PortID(PortNumber) )
import RemoteCommon          ( StressState(..) )

-- | open a server on port 8080
main :: IO ()
main =
    bracket
      (openLocalState $ StressState 0)
      closeAcidState
      (acidServer (sharedSecretCheck (singleton $ pack "12345")) (PortNumber 8080))
