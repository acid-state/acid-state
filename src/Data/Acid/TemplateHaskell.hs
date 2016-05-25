{-# LANGUAGE TemplateHaskell, CPP #-}
{- Holy crap this code is messy. -}
module Data.Acid.TemplateHaskell
    ( makeAcidic
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

import Data.Acid.Core
import Data.Acid.Common

import Data.List ((\\), nub)
import Data.Maybe (mapMaybe)
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
#if MIN_VERSION_template_haskell(2,11,0)
                 DataD _cxt _name tyvars _kind constructors _derivs
#else
                 DataD _cxt _name tyvars constructors _derivs
#endif
                   -> makeAcidic' eventNames stateName tyvars constructors
#if MIN_VERSION_template_haskell(2,11,0)
                 NewtypeD _cxt _name tyvars _kind constructor _derivs
#else
                 NewtypeD _cxt _name tyvars constructor _derivs
#endif
                   -> makeAcidic' eventNames stateName tyvars [constructor]
                 TySynD _name tyvars _ty
                   -> makeAcidic' eventNames stateName tyvars []
                 _ -> error "Unsupported state type. Only 'data', 'newtype' and 'type' are supported."
           _ -> error "Given state is not a type."

makeAcidic' :: [Name] -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
makeAcidic' eventNames stateName tyvars constructors
    = do events <- sequence [ makeEvent eventName | eventName <- eventNames ]
         acidic <- makeIsAcidic eventNames stateName tyvars constructors
         return $ acidic : concat events

makeEvent :: Name -> Q [Dec]
makeEvent eventName
    = do eventType <- getEventType eventName
         d <- makeEventDataType eventName eventType
         b <- makeSafeCopyInstance eventName eventType
         i <- makeMethodInstance eventName eventType
         e <- makeEventInstance eventName eventType
         return [d,b,i,e]

getEventType :: Name -> Q Type
getEventType eventName
    = do eventInfo <- reify eventName
         case eventInfo of
#if MIN_VERSION_template_haskell(2,11,0)
           VarI _name eventType _decl
#else
           VarI _name eventType _decl _fixity
#endif
             -> return eventType
           _ -> error $ "Events must be functions: " ++ show eventName

--instance (SafeCopy key, Typeable key, SafeCopy val, Typeable val) => IsAcidic State where
--  acidEvents = [ UpdateEvent (\(MyUpdateEvent arg1 arg2 -> myUpdateEvent arg1 arg2) ]
makeIsAcidic eventNames stateName tyvars constructors
    = do types <- mapM getEventType eventNames
         stateType' <- stateType
         let preds = [ ''SafeCopy, ''Typeable ]
             ty = appT (conT ''IsAcidic) stateType
             handlers = zipWith makeEventHandler eventNames types
             cxtFromEvents = nub $ concat $ zipWith (eventCxts stateType' tyvars) eventNames types
         cxts' <- mkCxtFromTyVars preds tyvars cxtFromEvents
         instanceD (return cxts') ty
                   [ valD (varP 'acidEvents) (normalB (listE handlers)) [] ]
    where stateType = foldl appT (conT stateName) (map varT (allTyVarBndrNames tyvars))

-- | This function analyses an event function and extracts any
-- additional class contexts which need to be added to the IsAcidic
-- instance.
--
-- For example, if we have:
--
-- > data State a = ...
--
-- > setState :: (Ord a) => a -> UpdateEvent (State a) ()
--
-- Then we need to generate an IsAcidic instance like:
--
-- > instance (SafeCopy a, Typeable a, Ord a) => IsAcidic (State a)
--
-- Note that we can only add constraints for type variables which
-- appear in the State type. If we tried to do this:
--
-- > setState :: (Ord a, Ord b) => a -> b -> UpdateEvent (State a) ()
--
-- We will get an ambigious type variable when trying to create the
-- 'IsAcidic' instance, because there is no way to figure out what
-- type 'b' should be.
--
-- The tricky part of this code is that we need to unify the type
-- variables.
--
-- Let's say the user writes their code using 'b' instead of 'a':
--
-- > setState :: (Ord b) => b -> UpdateEvent (State b) ()
--
-- In the 'IsAcidic' instance, we are still going to use 'a'. So we
-- need to rename the variables in the context to match.
--
-- The contexts returned by this function will have the variables renamed.
eventCxts :: Type        -- ^ State type (used for error messages)
          -> [TyVarBndr] -- ^ type variables that will be used for the State type in the IsAcidic instance
          -> Name        -- ^ 'Name' of the event
          -> Type        -- ^ 'Type' of the event
          -> [Pred]      -- ^ extra context to add to 'IsAcidic' instance
eventCxts targetStateType targetTyVars eventName eventType =
    let (_tyvars, cxt, _args, stateType, _resultType, _isUpdate)
                    = analyseType eventName eventType
        eventTyVars = findTyVars stateType -- find the type variable names that this event is using for the State type
        table       = zip eventTyVars (map tyVarBndrName targetTyVars) -- create a lookup table
    in map (unify table) cxt -- rename the type variables
    where
      -- | rename the type variables in a Pred
      unify :: [(Name, Name)] -> Pred -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
      unify table p = rename p table p -- in 2.10.0: type Pred = Type
#else
      unify table p@(ClassP n tys) = ClassP n (map (rename p table) tys)
      unify table p@(EqualP a b)   = EqualP (rename p table a) (rename p table b)
#endif

      -- | rename the type variables in a Type
      rename :: Pred -> [(Name, Name)] -> Type -> Type
      rename pred table t@(ForallT tyvarbndrs cxt typ) = -- this is probably wrong? I don't think acid-state can really handle this type anyway..
          ForallT (map renameTyVar tyvarbndrs) (map (unify table) cxt) (rename pred table typ)
          where
            renameTyVar (PlainTV name)    = PlainTV  (renameName pred table name)
            renameTyVar (KindedTV name k) = KindedTV (renameName pred table name) k
      rename pred table (VarT n)   = VarT $ renameName pred table n
      rename pred table (AppT a b) = AppT (rename pred table a) (rename pred table b)
      rename pred table (SigT a k) = SigT (rename pred table a) k
      rename _    _     typ        = typ

      -- | rename a 'Name'
      renameName :: Pred -> [(Name, Name)] -> Name -> Name
      renameName pred table n =
          case lookup n table of
            Nothing -> error $ unlines [ show $ ppr_sig eventName eventType
                                       , ""
                                       , "can not be used as an UpdateEvent because the class context: "
                                       , ""
                                       , pprint pred
                                       , ""
                                       , "contains a type variable which is not found in the state type: "
                                       , ""
                                       , pprint targetStateType
                                       , ""
                                       , "You may be able to fix this by providing a type signature that fixes these type variable(s)"
                                       ]
            (Just n') -> n'

-- UpdateEvent (\(MyUpdateEvent arg1 arg2) -> myUpdateEvent arg1 arg2)
makeEventHandler :: Name -> Type -> ExpQ
makeEventHandler eventName eventType
    = do assertTyVarsOk
         vars <- replicateM (length args) (newName "arg")
         let lamClause = conP eventStructName [varP var | var <- vars ]
         conE constr `appE` lamE [lamClause] (foldl appE (varE eventName) (map varE vars))
    where constr = if isUpdate then 'UpdateEvent else 'QueryEvent
          (tyvars, _cxt, args, stateType, _resultType, isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs
          stateTypeTyVars = findTyVars stateType
          tyVarNames = map tyVarBndrName tyvars
          assertTyVarsOk =
              case tyVarNames \\ stateTypeTyVars of
                [] -> return ()
                ns -> error $ unlines
                      [show $ ppr_sig eventName eventType
                      , ""
                      , "can not be used as an UpdateEvent because it contains the type variables: "
                      , ""
                      , pprint ns
                      , ""
                      , "which do not appear in the state type:"
                      , ""
                      , pprint stateType
                      ]



--data MyUpdateEvent = MyUpdateEvent Arg1 Arg2
--  deriving (Typeable)
makeEventDataType eventName eventType
    = do let con = normalC eventStructName [ strictType notStrict (return arg) | arg <- args ]
#if MIN_VERSION_template_haskell(2,11,0)
             cxt = mapM conT [''Typeable]
#else
             cxt = [''Typeable]
#endif
         case args of
#if MIN_VERSION_template_haskell(2,11,0)
          [_] -> newtypeD (return []) eventStructName tyvars Nothing con cxt
          _   -> dataD (return []) eventStructName tyvars Nothing [con] cxt
#else
          [_] -> newtypeD (return []) eventStructName tyvars con cxt
          _   -> dataD (return []) eventStructName tyvars [con] cxt
#endif
    where (tyvars, _cxt, args, _stateType, _resultType, _isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

-- instance (SafeCopy key, SafeCopy val) => SafeCopy (MyUpdateEvent key val) where
--    put (MyUpdateEvent a b) = do put a; put b
--    get = MyUpdateEvent <$> get <*> get
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
    where (tyvars, context, args, _stateType, _resultType, _isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

mkCxtFromTyVars preds tyvars extraContext
    = cxt $ [ classP classPred [varT tyvar] | tyvar <- allTyVarBndrNames tyvars, classPred <- preds ] ++
            map return extraContext

{-
instance (SafeCopy key, Typeable key
         ,SafeCopy val, Typeable val) => Method (MyUpdateEvent key val) where
  type MethodResult (MyUpdateEvent key val) = Return
  type MethodState (MyUpdateEvent key val) = State key val
-}
makeMethodInstance eventName eventType
    = do let preds = [ ''SafeCopy, ''Typeable ]
             ty = AppT (ConT ''Method) (foldl AppT (ConT eventStructName) (map VarT (allTyVarBndrNames tyvars)))
             structType = foldl appT (conT eventStructName) (map varT (allTyVarBndrNames tyvars))
         instanceD (cxt $ [ classP classPred [varT tyvar] | tyvar <- allTyVarBndrNames tyvars, classPred <- preds ] ++ map return context)
                   (return ty)
#if __GLASGOW_HASKELL__ >= 707
                   [ tySynInstD ''MethodResult (tySynEqn [structType] (return resultType))
                   , tySynInstD ''MethodState  (tySynEqn [structType] (return stateType))
#else
                   [ tySynInstD ''MethodResult [structType] (return resultType)
                   , tySynInstD ''MethodState  [structType] (return stateType)
#endif
                   ]
    where (tyvars, context, _args, stateType, resultType, _isUpdate) = analyseType eventName eventType
          eventStructName = mkName (structName (nameBase eventName))
          structName [] = []
          structName (x:xs) = toUpper x : xs

--instance (SafeCopy key, Typeable key
--         ,SafeCopy val, Typeable val) => UpdateEvent (MyUpdateEvent key val)
makeEventInstance eventName eventType
    = do let preds = [ ''SafeCopy, ''Typeable ]
             eventClass = if isUpdate then ''UpdateEvent else ''QueryEvent
             ty = AppT (ConT eventClass) (foldl AppT (ConT eventStructName) (map VarT (allTyVarBndrNames tyvars)))
         instanceD (cxt $ [ classP classPred [varT tyvar] | tyvar <- allTyVarBndrNames tyvars, classPred <- preds ] ++ map return context)
                   (return ty)
                   []
    where (tyvars, context, _args, _stateType, _resultType, isUpdate) = analyseType eventName eventType
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
                                  (binds, cxt, t')
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

-- | find the type variables
-- | e.g. State a b  ==> [a,b]
findTyVars :: Type -> [Name]
findTyVars (ForallT _ _ a) = findTyVars a
findTyVars (VarT n)   = [n]
findTyVars (AppT a b) = findTyVars a ++ findTyVars b
findTyVars (SigT a _) = findTyVars a
findTyVars _          = []

-- | extract the 'Name' from a 'TyVarBndr'
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n)    = n
tyVarBndrName (KindedTV n _) = n

allTyVarBndrNames :: [TyVarBndr] -> [Name]
allTyVarBndrNames tyvars = map tyVarBndrName tyvars
