{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Control.Exception     ( bracket )
import Data.Acid             ( openLocalState, closeAcidState )
import Data.Acid.Remote      ( sharedSecretCheck )
import Data.Acid.Remote.TLS  ( acidServerTLS )
import Data.ByteString.Char8 ( pack )
import Data.Set              ( singleton )
import Network               ( PortID(..) )
import Network.Socket        ( PortNumber(PortNum) )
import RemoteCommonTLS       ( StressState(..) )

-- | open a server on port 8080
main :: IO ()
main =
    let secrets = singleton $ pack "12345" in
    bracket
      (openLocalState $ StressState 0)
      closeAcidState
      -- do not use the provided 'ssl/test.crt' and 'ssl/test.key' in your real application.
      (acidServerTLS "ssl/test.crt" "ssl/test.key" (sharedSecretCheck secrets) (PortNumber 8080))
