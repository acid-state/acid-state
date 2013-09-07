{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Control.Monad         ( replicateM_ )
import Data.Acid             ( AcidState, closeAcidState, createCheckpoint, query, update )
import Data.Acid.Advanced    ( scheduleUpdate )
import Data.Acid.Remote      ( openRemoteState, sharedSecretPerform, createArchive )
import Data.ByteString.Char8 ( pack )
import Network               ( PortID(..) )
import RemoteCommon          ( StressState(..), ClearState(..), PokeState(..), QueryState(..) )
import System.Environment    ( getArgs )
import System.IO             ( hFlush, stdout )

------------------------------------------------------
-- printHelp

printHelp :: IO ()
printHelp
  = do putStrLn $ "Commands:"
       putStrLn $ "  query            Prints out the current state."
       putStrLn $ "  poke             Spawn 100k transactions."
       putStrLn $ "  checkpoint       Create a new checkpoint."
       putStrLn $ "  archive          Create archive."
       putStrLn $ "  clear            Clear the state and create a new checkpoint."
       putStrLn $ "  quit             Exit with out creating a checkpoint."

------------------------------------------------------
-- interactive command loop

commandLoop :: AcidState StressState -> IO ()
commandLoop acid
  = do printHelp
       go
    where
      go = do
        putStr "> "
        hFlush stdout
        cmd <- getLine
        case cmd of
          "checkpoint"
              -> do createCheckpoint acid
                    go
          "archive"
              -> do createArchive acid
                    go
          "query"
              -> do n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
                    go
          "poke"
              -> do putStr "Issuing 100k transactions... "
                    hFlush stdout
                    replicateM_ (100000-1) (scheduleUpdate acid PokeState)
                    update acid PokeState
                    putStrLn "Done"
                    go
          "clear"
              -> do update acid ClearState
                    createCheckpoint acid
                    go
          "quit"
              -> do closeAcidState acid
                    return ()

          _
              -> do printHelp
                    go

------------------------------------------------------
-- connect to remote server and start command-loop

main :: IO ()
main
  = do args <- getArgs
       case args of
         [] ->
             do acid <- openRemoteState (sharedSecretPerform $ (pack "12345")) "localhost" (PortNumber $ fromIntegral 8080)
                commandLoop acid

         [hostname, port] ->
             do acid <- openRemoteState (sharedSecretPerform $ (pack "12345")) hostname (PortNumber $ fromIntegral $ read port)
                commandLoop acid
         _ -> putStrLn "Usage: RemoteClientTLS [<hostname> <port>]"
