{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Acid.TemplateHaskellSpec where

import Test.Hspec hiding (context)

import Data.SafeCopy (SafeCopy)
import Data.Typeable (Typeable)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad.Reader
import Control.Monad.State

import Data.Acid
import Data.Acid.TemplateHaskell

spec :: Spec
spec = do
    let name = mkName "foo"
        nameT = ConT name
        upperName = mkName "Foo"
        upperNameT = ConT upperName

    describe "makeEventInstance" $ do
        it "works with monomorphic types" $ do
            eventType <- runQ [t| Int -> Query Char () |]
            makeEventInstance name eventType
                `quoteShouldBe`
                    [d| instance QueryEvent $(return upperNameT) |]

        it "requires instances on polymorphic types" $ do
            let a = VarT (mkName "a")
                a' = return a
            eventType <- runQ [t| (Ord $(a')) => $(a') -> Update Char $(a') |]

            makeEventInstance name eventType
                `quoteShouldBe`
                    [d| instance (Ord $(a')) => UpdateEvent $(return upperNameT)
                    |]


    describe "analyseType" $ do
        it "can work with the Query type" $ do
            typ <- runQ [t| Int -> Query String Char |]

            analyseType name typ
                `shouldBe` TypeAnalysis
                    { tyvars = []
                    , context = []
                    , argumentTypes = [ConT ''Int]
                    , stateType = ConT ''String
                    , resultType = ConT ''Char
                    , isUpdate = False
                    }

        it "can work with MonadReader" $ do
            typ <- runQ [t| forall m. (MonadReader Int m) => Int -> m () |]
            analyseType name typ
                `shouldBe` TypeAnalysis
                    { tyvars = []
                    , context = []
                    , argumentTypes = [ConT ''Int]
                    , stateType = ConT ''Int
                    , resultType = TupleT 0
                    , isUpdate = False
                    }

        it "can work with many type variables (note that eventCxts later rejects this)" $ do
            let m = mkName "m"
            typ <- runQ [t| (MonadReader Int $(varT m)) => Int -> Query Int ($(varT m) ()) |]
            analyseType name typ
                `shouldBe` TypeAnalysis
                    { tyvars = []
                    , context =
#if MIN_VERSION_template_haskell(2,10,0)
                        [ ConT ''MonadReader
                            `AppT` ConT ''Int
                            `AppT` VarT m
                        ]
#else
                        [ ClassP ''MonadReader [ConT ''Int, VarT m]
                        ]
#endif
                    , argumentTypes = [ConT ''Int]
                    , stateType = ConT ''Int
                    , resultType = VarT m `AppT` TupleT 0
                    , isUpdate = False
                    }

    describe "eventCxts" $ do
        let binders = []
            stateType = ConT ''Char
        it "rejects types with constrainted type variables unknown to state" $ do
            let predicate eventType =
                    evaluate
                        . force
                        . map show
                        $ eventCxts stateType binders name eventType
            eventType <- runQ [t| forall a. (Ord a) => Int -> Query Char a |]

            predicate eventType
                `shouldThrow`
                    anyErrorCall

        it "accepts types with unconstrained type variables" $ do
            eventType <- runQ [t| forall a. Int -> Query Char a |]

            eventCxts stateType binders name eventType
                `shouldBe`
                    []
        let x = mkName "x"

        it "accepts constrained type variables in the state" $ do
            let binders = [PlainTV (mkName "x")]
                stateType = ConT ''Maybe `AppT` VarT x
            eventType <- runQ [t| forall a. (Ord a) => Int -> Query (Maybe a) Int|]

            eventCxts stateType binders name eventType
                `shouldBe`
#if MIN_VERSION_template_haskell(2,10,0)
                    [ConT ''Ord `AppT` VarT x]
#else
                    [ClassP ''Ord [VarT x]]
#endif

        it "can rename a polymorphic state" $ do
            eventType <- runQ [t| forall r m. (MonadReader r m, Ord r) => Int -> m Char |]
            eventCxts stateType binders name eventType
                `shouldBe`
#if MIN_VERSION_template_haskell(2,10,0)
                    [ConT ''Ord `AppT` ConT ''Char]
#else
                    [ClassP ''Ord [ConT ''Char]]
#endif


quoteShouldBe :: (Eq a, Show a) => Q a -> Q [a] -> Expectation
quoteShouldBe qa qb = do
    actual <- runQ qa
    [expected] <- runQ qb
    actual `shouldBe` expected
