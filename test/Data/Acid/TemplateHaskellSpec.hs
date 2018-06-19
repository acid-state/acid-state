{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Acid.TemplateHaskellSpec where

import Test.Hspec

import Data.Acid
import Data.Acid.TemplateHaskell
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad.Reader
import Control.Monad.State

spec :: Spec
spec = do
    describe "analyseType" $ do
        it "can work with the Query type" $ do
            let typs = [ConT ''Int]
                name = mkName "foo"
                isUpdate = False
                stateType = ConT ''String
                resultType = ConT ''Char
                args = [ConT ''Int]
                ctx = []
                tvb = []
            typ <- runQ [t| Int -> Query String Char |]

            analyseType name typ
                `shouldBe`
                    ( tvb
                    , ctx
                    , typs
                    , stateType
                    , resultType
                    , isUpdate
                    )

        it "can work with MonadReader" $ do
            let typs = [ConT ''Int]
                name = mkName "foo"
                isUpdate = False
                stateType = ConT ''Int
                resultType = TupleT 0
                args = [ConT ''Int]
                ctx = []
                tvb = []
            typ <- runQ [t| (MonadReader Int m) => Int -> m () |]
            analyseType name typ
                `shouldBe`
                    ( tvb
                    , ctx
                    , typs
                    , stateType
                    , resultType
                    , isUpdate
                    )

        it "can work with many type variables" $ do
            let typs = [ConT ''Int]
                name = mkName "foo"
                isUpdate = False
                stateType = ConT ''Int
                resultType = VarT m `AppT` TupleT 0
                args = [ConT ''Int]
                m = mkName "m"
                ctx =
#if MIN_VERSION_template_haskell(2,10,0)
                    [ ConT ''MonadReader
                        `AppT` ConT ''Int
                        `AppT` VarT m
                    ]
#else
                    [ ClassP ''MonadReader [ConT ''Int, VarT m]
                    ]
#endif

                tvb = []
            typ <- runQ [t| (MonadReader Int $(varT m)) => Int -> Query Int ($(varT m) ()) |]
            analyseType name typ
                `shouldBe`
                    ( tvb
                    , ctx
                    , typs
                    , stateType
                    , resultType
                    , isUpdate
                    )
