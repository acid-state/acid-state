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

-- | Query the current value of the state.  This is not used in the
-- generated commands, but is used for checking the state we get back
-- in 'prop_restore_old_state_1' etc.
askState :: Query KeyValue KeyValue
askState = ask

$(makeAcidic ''KeyValue ['insertKey, 'reverseKey, 'lookupKey, 'askState])

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
genKey = Gen.int (Range.constant 1 10)

genValue :: Gen Value
genValue = Gen.string (Range.constant 0 10) Gen.alphaNum

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

prop_restore_old_state_1 :: Property
prop_restore_old_state_1 = restoreOldStateProperty (acidStateInterface fp) (KeyValue Map.empty) AskState r
  where
    fp = "test-state/OldStateTest1"
    r  = KeyValue (Map.fromList [(1,""),(2,""),(3,"y5Pl"),(4,""),(5,"Zc"),(6,"8aENKK")
                                ,(7,"FDzyGCz"),(8,""),(9,"xq"),(10,"1Ra1obuINa")])

prop_restore_old_state_2 :: Property
prop_restore_old_state_2 = restoreOldStateProperty (acidStateInterface fp) (KeyValue Map.empty) AskState r
  where
    fp = "test-state/OldStateTest2"
    r  = KeyValue (Map.fromList [(1,"PLwR1S6F"),(2,"0yrcVQM0c"),(3,"zAA"),(4,"prAocOc")
                                ,(5,"HM"),(6,"ENdfLrrW"),(7,"sESXGsI"),(8,"AFa69uu5")
                                ,(9,"XBvIQHX"),(10,"A2CzkvW")])

prop_restore_old_state_3 :: Property
prop_restore_old_state_3 = restoreOldStateProperty (acidStateInterface fp) (KeyValue Map.empty) AskState r
  where
    fp = "test-state/OldStateTest3"
    r  = KeyValue (Map.fromList [(1,"4"),(2,"RQ1xEc5"),(3,"aT0Hqk"),(4,"Duf")
                                ,(5,"tssCng7e0d"),(6,"uQW0hCVze"),(7,"FSCZPMGL")
                                ,(8,"q1WI9He"),(9,"IYHbWmO"),(10,"lCErPJC3")])


tests :: IO Bool
tests = checkParallel $$(discover)
