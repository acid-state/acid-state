module RemoteServer where

import           Control.Exception (bracket)
import           Data.Acid         (closeAcidState, openLocalState)
import           Data.Acid.Remote  (acidServer, acidServerSockAddr, skipAuthenticationCheck)
import           RemoteCommon      (StressState (..))
import           Network.Socket    (SockAddr(..))

main :: IO ()
main = bracket (openLocalState $ StressState 0)
         closeAcidState $ acidServer skipAuthenticationCheck 8080
-- on Unixy systems we could use a Unix Domain Socket
--       closeAcidState $ acidServerSockAddr skipAuthenticationCheck (SockAddrUnix "remote.socket")
