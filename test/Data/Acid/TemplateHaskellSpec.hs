{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Acid.TemplateHaskellSpec where

import Test.Hspec hiding (context)

import Data.Acid
import Data.Acid.TemplateHaskell
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad.Reader
import Control.Monad.State

spec :: Spec
spec = do
    describe "analyseType" $ do
        let name = mkName "foo"
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
            typ <- runQ [t| (MonadReader Int m) => Int -> m () |]
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
