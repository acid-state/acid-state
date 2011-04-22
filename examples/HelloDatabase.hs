{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Main (main) where

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import System.Environment                    ( getArgs )
import Data.SafeCopy

type Message = String
data Database = Database [Message]

$(deriveSafeCopy 0 'base ''Database)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.                                                                                                                                    
addMessage :: Message -> Update Database ()
addMessage msg
    = do Database messages <- get
         put $ Database (msg:messages)

viewMessages :: Int -> Query Database [Message]
viewMessages limit
    = do Database messages <- ask
         return $ take limit messages

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''Database ['addMessage, 'viewMessages])

main :: IO ()
main = do args <- getArgs
          database <- openAcidStateFrom "myDatabase/" (Database ["Welcome to the acid-state database."])
          if null args
            then do messages <- query database (ViewMessages 10)
                    putStrLn "Last 10 messages:"
                    mapM_ putStrLn [ "  " ++ message | message <- messages ]
            else do update database (AddMessage (unwords args))
                    putStrLn "Your message has been added to the database."
