module RemoteServer where

import           Control.Exception (bracket)
import           Data.Acid         (closeAcidState, openLocalState)
import           Data.Acid.Remote  (acidServer, skipAuthenticationCheck)
import           RemoteCommon      (StressState (..))

main :: IO ()
main = bracket (openLocalState $ StressState 0)
       closeAcidState $ acidServer skipAuthenticationCheck 8080
