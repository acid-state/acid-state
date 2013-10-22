{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid.Remote.TLS
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com, jeremy@n-heptane.com
 Portability :  non-portable (uses GHC extensions)

This module provides the same functionality as "Data.Acid.Remote" but
over a secured TLS socket.

-}
module Data.Acid.Remote.TLS
    (
    -- * Server/Client
      acidServerTLS
    , openRemoteStateTLS
    -- * Authentication
    , skipAuthenticationCheck
    , skipAuthenticationPerform
    , sharedSecretCheck
    , sharedSecretPerform

    ) where

import Control.Concurrent        ( forkIO, threadDelay )
import Control.Exception         ( Handler(..), IOException, SomeException, catch, catches, handle
                                 , finally, throwIO )
import Control.Monad             ( forever, when )
import Data.Acid                 ( AcidState, IsAcidic )
import Data.Acid.Remote          ( CommChannel(..), process, processRemoteState, skipAuthenticationCheck
                                 , skipAuthenticationPerform, sharedSecretCheck, sharedSecretPerform )
import Data.SafeCopy             ( SafeCopy )
import GHC.IO.Exception          ( IOErrorType(..) )
import           OpenSSL         ( withOpenSSL )
import           OpenSSL.Session ( SomeSSLException, SSL, SSLContext )
import qualified OpenSSL.Session as SSL
import Network                   ( HostName, PortID(..), Socket, listenOn, sClose, withSocketsDo )
import Network.Socket            as Socket ( Family(..), SockAddr(..), SocketType(..), accept, socket, connect )
import Network.BSD               ( getHostByName, getProtocolNumber, getServicePortNumber, hostAddress )
import System.Directory          ( removeFile )
import System.IO.Error           ( ioeGetErrorType, isFullError, isDoesNotExistError )

debugStrLn :: String -> IO ()
debugStrLn s =
    do putStrLn s -- uncomment to enable debugging
       return ()

initSSLContext :: FilePath  -- ^ path to ssl certificate
               -> FilePath  -- ^ path to ssl private key
               -> IO SSLContext
initSSLContext cert key =
    do ctx <- SSL.context
       SSL.contextSetPrivateKeyFile  ctx key
       SSL.contextSetCertificateFile ctx cert
       SSL.contextSetDefaultCiphers  ctx

       certOk <- SSL.contextCheckPrivateKey ctx
       when (not certOk) $ error $ "OpenTLS certificate and key do not match."

       return ctx

-- | accept a TLS connection
acceptTLS :: SSLContext -> Socket -> IO (Socket, SSL, SockAddr)
acceptTLS ctx sck' =
    do -- do normal accept
      (sck, sockAddr) <- accept sck'
      --  then TLS accept
      handle (\ (e :: SomeException) -> sClose sck >> throwIO e) $ do
          ssl <- SSL.connection ctx sck
          SSL.accept ssl
          return (sck, ssl, sockAddr)

{- | Accept connections on @port@ and handle requests using the given 'AcidState'.
     This call doesn't return.

     The connection is secured using TLS/SSL.

     On Unix®-like systems you can use 'UnixSocket' to communicate
     using a socket file. To control access, you can set the permissions of
     the parent directory which contains the socket file.

     see also: 'openRemoteStateTLS' and 'sharedSecretCheck'.
 -}
acidServerTLS :: SafeCopy st =>
                 FilePath                 -- ^ path to ssl certificate
              -> FilePath                 -- ^ path to ssl private key
              -> (CommChannel -> IO Bool) -- ^ authorization function
              -> PortID                   -- ^ port to list on
              -> AcidState st             -- ^ 'AcidState' to serve
              -> IO ()
acidServerTLS sslCert sslKey checkAuth port acidState
  = withSocketsDo $
    do withOpenSSL $ return ()
       debugStrLn $ "acidServerTLS: listenOn " ++ show port
       tlsSocket <- listenOn port
       debugStrLn $ "acidServerTLS: initSSLContext"
       ctx       <- initSSLContext sslCert sslKey
       let worker :: (Socket, SSL, SockAddr) -> IO ()
           worker (socket, ssl, _sockAddr) =
               do -- TODO: log this connection, sockAddr
                  let socketCommChannel :: CommChannel
                      socketCommChannel = CommChannel
                        { ccPut     = SSL.write ssl
                        , ccGetSome = SSL.read ssl
                        , ccClose   = shutdownClose socket ssl
                        }
                  forkIO $ (do authorized <- checkAuth socketCommChannel
                               when authorized $
                                    ignoreSome $ (process socketCommChannel acidState)
                               ccClose socketCommChannel) `catch` (\(e::SomeException) -> do
                                                                     shutdownClose socket ssl
                                                                     throwIO e)
                  return ()
           loop :: IO ()
           loop = do ignoreSome $ (forever $ worker =<< acceptTLS ctx tlsSocket)
                     loop
       loop `finally` (cleanup tlsSocket `catch` ignoreException)

    where
      cleanup tlsSocket
        = do debugStrLn "acidServerTLS: cleanup."
             sClose tlsSocket
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
             case port of
               (UnixSocket path) ->
                   removeFile path `catch` (\e -> if isDoesNotExistError e then return () else throwIO e)
               _ -> return ()
#endif

      -- exception handlers
      ignoreConnectionAbruptlyTerminated :: SSL.ConnectionAbruptlyTerminated -> IO ()
      ignoreConnectionAbruptlyTerminated _ = return ()

      ignoreSSLException :: SSL.SomeSSLException -> IO ()
      ignoreSSLException _ = return ()

      ignoreException :: SomeException -> IO ()
      ignoreException _ = return ()

      shutdownClose :: Socket -> SSL -> IO ()
      shutdownClose socket ssl =
          do debugStrLn "acidServerTLS: shutdownClose."
             SSL.shutdown ssl SSL.Unidirectional `catch` ignoreException
             sClose socket `catch` ignoreException

      ignoreSome op =
               op `catches` [ Handler $ ignoreSSLException
                            , Handler $ ignoreConnectionAbruptlyTerminated
                            , Handler $ \(e :: IOException)    ->
                                  if isFullError e || isDoesNotExistError e || isResourceVanishedError e
                                  then return ()
                                  else throwIO e
                              ]

      isResourceVanishedError :: IOException -> Bool
      isResourceVanishedError = isResourceVanishedType . ioeGetErrorType

      isResourceVanishedType :: IOErrorType -> Bool
      isResourceVanishedType ResourceVanished = True
      isResourceVanishedType _                = False

{- | Connect to an acid-state server which is sharing an 'AcidState'.

The connection is secured using SSL/TLS.
 -}
openRemoteStateTLS  :: IsAcidic st =>
                       (CommChannel -> IO ()) -- ^ authentication function, see 'sharedSecretPerform'
                    -> HostName               -- ^ remote host to connect to (ignored when 'PortID' is 'UnixSocket')
                    -> PortID                 -- ^ remote port to connect to
                    -> IO (AcidState st)
openRemoteStateTLS performAuthorization host port
  = do withOpenSSL $ return ()
       processRemoteState reconnect
    where
      sslCommChannel ssl =
         CommChannel { ccGetSome = SSL.read ssl
                     , ccPut     = SSL.write ssl
                     , ccClose   = do SSL.shutdown ssl SSL.Unidirectional
                                      --   close ssl
                     }

      -- | reconnect
      reconnect :: IO CommChannel
      reconnect
          = (do ssl <- connectToTLS host port
                let cc = sslCommChannel ssl
                performAuthorization cc
                return cc
            )
            `catch`
            ((\e -> threadDelay 1000000 >> reconnect) :: IOError -> IO CommChannel)

-- IPV4 support only, sorry
connectToTLS :: HostName
             -> PortID
             -> IO SSL
connectToTLS hostName (Service serv)
  = do port <- getServicePortNumber serv
       connectToTLS hostName (PortNumber port)

connectToTLS hostName (PortNumber port)
  = do proto <- getProtocolNumber "tcp"
       sock <- socket AF_INET Stream proto
       (do he <- getHostByName hostName
           Socket.connect sock (SockAddrInet port (hostAddress he))
           ctx <- SSL.context
           ssl <- SSL.connection ctx sock
           SSL.connect ssl
           return ssl) `catch` (\e -> do print (e :: SomeSSLException)
                                         sClose sock
                                         throwIO e
                               )
connectToTLS _hostName p@(UnixSocket path)
  = do debugStrLn $ "connectToTLS: " ++ show p
       sock <- socket AF_UNIX Stream 0
       (do debugStrLn $ "connectToTLS: connect."
           Socket.connect sock (SockAddrUnix path)
           ctx <- SSL.context
           ssl <- SSL.connection ctx sock
           debugStrLn $ "connectToTLS: connect ssl."
           SSL.connect ssl
           debugStrLn $ "connectToTLS: done."
           return ssl) `catch` (\e -> do print (e :: SomeSSLException)
                                         sClose sock
                                         throwIO e
                               )

