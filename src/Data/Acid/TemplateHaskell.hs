{-# LANGUAGE TemplateHaskell #-}
{- Holy crap this code is messy. -}
module Data.Acid.TemplateHaskell
    ( makeAcidic
    ) where

import Language.Haskell.TH

import Data.Acid.Core
import Data.Acid.Local

import Data.SafeCopy
import Data.Typeable
import Data.Char
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
makeAcidic stateName eventNames
    = do stateInfo <- reify stateName
         case stateInfo of
           TyConI tycon
             ->case tycon of
                 DataD _cxt _name tyvars constructors _derivs
                   -> makeAcidic' eventNames stateName tyvars constructors
                 NewtypeD _cxt _name tyvars constructor _derivs
                   -> makeAcidic' eventNames stateName tyvars [constructor]
                 _ -> error "Unsupported state type. Only 'data' and 'newtype' are supported."
           _ -> error "Given state is not a type."

makeEvent :: Name -> Q [Dec]
makeEvent eventName
    = do eventInfo <- reify eventName
         eventType <- getEventType eventName
         d <- makeEventDataType eventName eventType
         b <- makeSafeCopyInstance eventName eventType
         i <- makeMethodInstance eventName eventType
         e <- makeEventInstance eventName eventType
         return [d,b,i,e]

getEventType :: Name -> Q Type
getEventType eventName
    = do eventInfo <- reify eventName
         case eventInfo of
           VarI _name eventType _decl _fixity
             -> return eventType
           _ -> error $ "Events must be functions: " ++ show eventName

--instance (SafeCopy key, Typeable key, SafeCopy val, Typeable val) => IsAcidic State where
--  acidEvents = [ UpdateEven (\(MyUpdateEvent arg1 arg2 -> myUpdateEvent arg1 arg2) ]
makeIsAcidic eventNames stateName tyvars constructors
    = do types <- mapM getEventType eventNames
         let preds = [ ''SafeCopy, ''Typeable ]
             ty = appT (conT ''IsAcidic) stateType
             handlers = map (uncurry makeEventHandler) (zip eventNames types)
         instanceD (mkCxtFromTyVars preds tyvars []) ty
                   [ valD (varP 'acidEvents) (normalB (listE handlers)) [] ]
    where stateType = foldl appT (conT stateName) [ varT var | PlainTV var <- tyvars ]

-- UpdateEvent (\(MyUpdateEvent arg1 arg2) -> myUpdateEvent arg1 arg2)
makeEventHandler :: Name -> Type -> ExpQ
makeEventHandler eventName eventType
    = do vars <- replicateM (length args) (newName "arg")
         let lamClause = conP eventStructName [varP var | var <- vars ]
         conE constr `appE` lamE [lamClause] (foldl appE (varE eventName) (map varE vars))
    where constr = if isUpdate then 'UpdateEvent else 'QueryEvent
          (tyvars, cxt, args, stateType, resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

--data MyUpdateEvent = MyUpdateEvent Arg1 Arg2
--  deriving (Typeable)
makeEventDataType eventName eventType
    = do let con = normalC eventStructName [ strictType notStrict (return arg) | arg <- args ]
         dataD (return cxt) eventStructName tyvars [con] [''Typeable]
    where (tyvars, cxt, args, stateType, resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

-- instance (SafeCopy key, SafeCopy val) => SafeCopy (MyUpdateEvent key val) where
--    put (MyUpdateEvent a b) = do put a; put b
--    get = MyUpdateEvent <$> get <*> get
makeSafeCopyInstance eventName eventType
    = do let preds = [ ''SafeCopy ]
             ty = AppT (ConT ''SafeCopy) (foldl AppT (ConT eventStructName) [ VarT tyvar | PlainTV tyvar <- tyvars ])

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
    where (tyvars, context, args, stateType, resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

mkCxtFromTyVars preds tyvars extraContext
    = cxt $ [ classP classPred [varT tyvar] | PlainTV tyvar <- tyvars, classPred <- preds ] ++
            map return extraContext

{-
instance (SafeCopy key, Typeable key
         ,SafeCopy val, Typeable val) => Method (MyUpdateEvent key val) where
  type MethodResult (MyUpdateEvent key val) = Return
  type MethodState (MyUpdateEvent key val) = State key val
-}
makeMethodInstance eventName eventType
    = do let preds = [ ''SafeCopy, ''Typeable ]
             ty = AppT (ConT ''Method) (foldl AppT (ConT eventStructName) [ VarT tyvar | PlainTV tyvar <- tyvars ])
             structType = foldl appT (conT eventStructName) [ varT tyvar | PlainTV tyvar <- tyvars ]
         instanceD (cxt $ [ classP classPred [varT tyvar] | PlainTV tyvar <- tyvars, classPred <- preds ] ++ map return context)
                   (return ty)
                   [ tySynInstD ''MethodResult [structType] (return resultType)
                   , tySynInstD ''MethodState  [structType] (return stateType)
                   ]
    where (tyvars, context, args, stateType, resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

--instance (SafeCopy key, Typeable key
--         ,SafeCopy val, Typeable val) => UpdateEvent (MyUpdateEvent key val)
makeEventInstance eventName eventType
    = do let preds = [ ''SafeCopy, ''Typeable ]
             eventClass = if isUpdate then ''UpdateEvent else ''QueryEvent
             ty = AppT (ConT eventClass) (foldl AppT (ConT eventStructName) [ VarT tyvar | PlainTV tyvar <- tyvars ])
             structType = foldl appT (conT eventStructName) [ varT tyvar | PlainTV tyvar <- tyvars ]
         instanceD (cxt $ [ classP classPred [varT tyvar] | PlainTV tyvar <- tyvars, classPred <- preds ] ++ map return context)
                   (return ty)
                   []
    where (tyvars, context, args, stateType, resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs


-- (tyvars, cxt, args, state type, result type, is update)
analyseType :: Name -> Type -> ([TyVarBndr], Cxt, [Type], Type, Type, Bool)
analyseType eventName t
    = let (tyvars, cxt, t') = case t of
                                ForallT binds [] t' -> 
                                  (binds, [], t')
                                ForallT binds cxt t' -> 
                                  error $ "Context restrictions on events aren't supported yet: " ++ show eventName
                                _ -> ([], [], t)
          args = getArgs t'
          (stateType, resultType, isUpdate) = findMonad t'
      in (tyvars, cxt, args, stateType, resultType, isUpdate)
    where getArgs ForallT{} = error $ "Event has an invalid type signature: Nested forall: " ++ show eventName
          getArgs (AppT (AppT ArrowT a) b) = a : getArgs b
          getArgs _ = []

          findMonad (AppT (AppT ArrowT a) b)
              = findMonad b
          findMonad (AppT (AppT (ConT con) state) result)
              | con == ''Update = (state, result, True)
              | con == ''Query  = (state, result, False)
          findMonad _ = error $ "Event has an invalid type signature: Not an Update or a Query: " ++ show eventName

makeAcidic' :: [Name] -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
makeAcidic' eventNames stateName tyvars constructors
    = do events <- sequence [ makeEvent eventName | eventName <- eventNames ]
         acidic <- makeIsAcidic eventNames stateName tyvars constructors
         return $ acidic : concat events
