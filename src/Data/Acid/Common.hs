{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Common
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- Common structures used by the various backends (local, memory).
--
module Data.Acid.Common where

import Data.Acid.Core

import Control.Monad
import Control.Monad.State   (MonadState, get, State)
import Control.Monad.Reader  (MonadReader, Reader, runReader)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif


class IsAcidic st where
    acidEvents :: [Event st]
      -- ^ List of events capable of updating or querying the state.


-- | Context monad for Update events.
newtype Update st a = Update { unUpdate :: State st a }
    deriving (Monad, Functor, MonadState st)

-- mtl pre-2.0 doesn't have these instances to newtype-derive, but they're
-- simple enough.
instance Applicative (Update st) where
    pure = return
    (<*>) = ap

-- | Context monad for Query events.
newtype Query st a  = Query { unQuery :: Reader st a }
    deriving (Monad, Functor, MonadReader st)

instance Applicative (Query st) where
    pure = return
    (<*>) = ap

-- | Run a query in the Update Monad.
liftQuery :: Query st a -> Update st a
liftQuery query
    = do st <- get
         return (runReader (unQuery query) st)

-- | Events return the same thing as Methods. The exact type of 'EventResult'
--   depends on the event.
type EventResult ev = MethodResult ev

type EventState ev = MethodState ev

-- | We distinguish between events that modify the state and those that do not.
--
--   UpdateEvents are executed in a MonadState context and have to be serialized
--   to disk before they are considered durable.
--
--   QueryEvents are executed in a MonadReader context and obviously do not have
--   to be serialized to disk.
data Event st where
    UpdateEvent :: UpdateEvent ev => (ev -> Update (EventState ev) (EventResult ev)) -> MethodSerialiser ev -> Event (EventState ev)
    QueryEvent  :: QueryEvent  ev => (ev -> Query (EventState ev) (EventResult ev)) -> MethodSerialiser ev -> Event (EventState ev)


-- | All UpdateEvents are also Methods.
class Method ev => UpdateEvent ev
-- | All QueryEvents are also Methods.
class Method ev => QueryEvent ev


eventsToMethods :: [Event st] -> [MethodContainer st]
eventsToMethods = map worker
    where worker :: Event st -> MethodContainer st
          worker (UpdateEvent fn ms) = Method (unUpdate . fn) ms
          worker (QueryEvent  fn ms) = Method (\ev -> do st <- get
                                                         return (runReader (unQuery $ fn ev) st)
                                              ) ms
