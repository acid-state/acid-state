{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Memory.Pure
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- AcidState container without a transaction log. Mostly used for testing.
--
-- This module consists of internal implementation details for
-- "Data.Acid.Memory".  You should not normally need to import it.  Call
-- 'Data.Acid.Memory.openMemoryState' and thereafter use the API from
-- "Data.Acid" instead.
--

module Data.Acid.Memory.Pure
    ( IsAcidic(..)
    , AcidState
    , Event(..)
    , EventResult
    , EventState
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , openAcidState
    , update
    , update_
    , query
    , liftQuery
    , runUpdate
    , runQuery
    ) where

import Data.Acid.Core
import Data.Acid.Common

import Control.Monad.State
import Control.Monad.Reader

{-| Pure state value used internally. This is not the same as
  'Data.Acid.AcidState' from "Data.Acid".
-}
data AcidState st
    = AcidState { localMethods :: MethodMap st
                , localState   :: st
                }

-- | Issue an Update event and wait for its result. Once this call returns, you are
--   guaranteed that the changes to the state are durable. Events may be issued in
--   parallel.
--
--   It's a run-time error to issue events that aren't supported by the AcidState.
update :: UpdateEvent event => AcidState (EventState event) -> event -> ( AcidState (EventState event)
                                                                        , EventResult event)
update acidState event
    = case runState hotMethod (localState acidState) of
        !(result, !newState) -> ( acidState { localState = newState }
                                , result )
    where hotMethod = lookupHotMethod (localMethods acidState) event

-- | Same as 'update' but ignoring the event result.
update_ :: UpdateEvent event => AcidState (EventState event) -> event -> AcidState (EventState event)
update_ acidState event
    = fst (update acidState event)

-- | Issue a Query event and wait for its result.
query  :: QueryEvent event  => AcidState (EventState event) -> event -> EventResult event
query acidState event
    = case runState hotMethod (localState acidState) of
        !(result, !_st) -> result
    where hotMethod = lookupHotMethod (localMethods acidState) event

-- | Create an AcidState given an initial value.
openAcidState :: IsAcidic st
              => st                          -- ^ Initial state value.
              -> AcidState st
openAcidState initialState
    = AcidState { localMethods = mkMethodMap (eventsToMethods acidEvents)
                , localState   = initialState }

-- | Execute the 'Update' monad in a pure environment.
runUpdate :: Update s r -> s -> (r, s)
runUpdate update = runState $ unUpdate update

-- | Execute the 'Query' monad in a pure environment.
runQuery :: Query s r -> s -> r
runQuery query = runReader $ unQuery query
