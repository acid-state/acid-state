{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module instantiates the general framework in
-- 'Data.Acid.StateMachineTest' with an acid-state component that
-- implements a simple key-value store.
module Data.Acid.KeyValueStateMachine (tests) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Acid.StateMachineTest
import           Data.SafeCopy
import qualified Data.Map as Map
import           Data.Typeable
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


type Key = Int
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

-- | Insert a key into the key-value store.
insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

-- | A slightly more complicated update transaction: reverse the value
-- at the given key, and return the resulting state.  Crucially, this
-- is non-idempotent, unlike 'insertKey'.
reverseKey :: Key -> Update KeyValue KeyValue
reverseKey key
    = do KeyValue m <- get
         let r = KeyValue (Map.adjust reverse key m)
         put r
         return r

-- | Look up a key from the store.
lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'reverseKey, 'lookupKey])

deriving instance Show InsertKey
deriving instance Show ReverseKey
deriving instance Show LookupKey

genInsertKey :: Gen InsertKey
genInsertKey = InsertKey <$> genKey <*> genValue

genReverseKey :: Gen ReverseKey
genReverseKey = ReverseKey <$> genKey

genLookupKey :: Gen LookupKey
genLookupKey = LookupKey <$> genKey

genKey :: Gen Key
genKey = Gen.int (Range.linear 1 10)

genValue :: Gen Value
genValue = Gen.string (Range.linear 0 10) Gen.unicode

keyValueCommands :: MonadIO m => [Command Gen m (Model KeyValue)]
keyValueCommands = [ acidUpdate genInsertKey
                   , acidUpdate genReverseKey
                   , acidQuery  genLookupKey
                   ]

-- | Possible initial states; because of #20 we can currently only use
-- one of these when testing the properties.
initialStates :: [KeyValue]
initialStates = [ KeyValue Map.empty
                , KeyValue (Map.singleton 1 "foo")
                ]

prop_sequential :: Property
prop_sequential = acidStateSequentialProperty (acidStateInterface fp) (pure (head initialStates)) keyValueCommands
  where
    fp = "state/KeyValueSequentialTest"

prop_parallel :: Property
prop_parallel = acidStateParallelProperty (acidStateInterface fp) (pure (head initialStates)) keyValueCommands
  where
    fp = "state/KeyValueParallelTest"


tests :: IO Bool
tests = checkParallel $$(discover)
