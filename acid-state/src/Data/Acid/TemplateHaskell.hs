{-# LANGUAGE TemplateHaskell, CPP, NamedFieldPuns #-}

{- Holy crap this code is messy. -}
module Data.Acid.TemplateHaskell
  ( module Data.Acid.TemplateHaskell
  , module Data.Acid.Internal.TemplateHaskell
  ) where

import Language.Haskell.TH

import Data.Acid.Core
import Data.Acid.Internal.TemplateHaskell

import Data.SafeCopy
import Control.Applicative
import Control.Monad

{-| Create the control structures required for acid states
    using Template Haskell.

This code:

@
myUpdate :: Argument -> Update State Result
myUpdate arg = ...

myQuery :: Argument -> Query State Result
myQuery arg = ...

$(makeAcidic ''State ['myUpdate, 'myQuery])
@

will make @State@ an instance of 'IsAcidic' and provide the following
events:

@
data MyUpdate = MyUpdate Argument
data MyQuery  = MyQuery Argument
@

-}
makeAcidic :: Name -> [Name] -> Q [Dec]
makeAcidic = makeAcidicWithSerialiser safeCopySerialiserSpec

-- | Default implementation of 'SerialiserSpec' that uses 'SafeCopy'
-- for serialising events.
safeCopySerialiserSpec :: SerialiserSpec
safeCopySerialiserSpec =
    SerialiserSpec { serialisationClassName = ''SafeCopy
                   , methodSerialiserName   = 'safeCopyMethodSerialiser
                   , makeEventSerialiser    = makeSafeCopyInstance
                   }

-- instance (SafeCopy key, SafeCopy val) => SafeCopy (MyUpdateEvent key val) where
--    put (MyUpdateEvent a b) = do put a; put b
--    get = MyUpdateEvent <$> get <*> get
makeSafeCopyInstance :: Name -> Type -> DecQ
makeSafeCopyInstance eventName eventType
    = do let preds = [ ''SafeCopy ]
             ty = AppT (ConT ''SafeCopy) (foldl AppT (ConT eventStructName) (map VarT (allTyVarBndrNames tyvars)))

             getBase = appE (varE 'return) (conE eventStructName)
             getArgs = foldl (\a b -> infixE (Just a) (varE '(<*>)) (Just (varE 'safeGet))) getBase args
             contained val = varE 'contain `appE` val

         putVars <- replicateM (length args) (newName "arg")
         let putClause = conP eventStructName [varP var | var <- putVars ]
             putExp    = doE $ [ noBindS $ appE (varE 'safePut) (varE var) | var <- putVars ] ++
                               [ noBindS $ appE (varE 'return) (tupE []) ]

         instanceD (mkCxtFromTyVars preds tyvars context)
                   (return ty)
                   [ funD 'putCopy [clause [putClause] (normalB (contained putExp)) []]
                   , valD (varP 'getCopy) (normalB (contained getArgs)) []
                   ]
    where TypeAnalysis { tyvars, context, argumentTypes = args } = analyseType eventName eventType
          eventStructName = toStructName eventName
