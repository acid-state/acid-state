{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid (makeAcidic)
import Data.Acid.Local
import Data.Acid.Remote (acidServer)

import Control.Exception (bracket)
import Data.Typeable

import Network

import RemoteCommon

-- open a server on port 8080

main :: IO ()
main =
    bracket
      (openAcidState $ StressState 0)
      closeAcidState
      (\s -> acidServer s (PortNumber 8080))
