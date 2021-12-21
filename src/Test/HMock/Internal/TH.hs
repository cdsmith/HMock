{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Template Haskell utilities used to implement HMock.
module Test.HMock.Internal.TH
  ( unappliedName,
    tvName,
    bindVar,
    substTypeVar,
    substTypeVars,
    splitType,
    freeTypeVars,
    relevantContext,
    constrainVars,
    unifyTypes,
    removeModNames,
    hasPolyType,
    hasNestedPolyType,
    resolveInstance,
    resolveInstanceType,
    simplifyContext,
    localizeMember,
  )
where

import Control.Monad.Extra (mapMaybeM, concatMapM)
import Data.Generics
import Data.List ((\\), nub)
import Data.Maybe (catMaybes, fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (NameFlavour (..))
import Test.HMock.Internal.Util (choices)

#if MIN_VERSION_template_haskell(2,17,0)

-- | Fetches the 'Name' of a 'TyVarBndr'.
tvName :: TyVarBndr flag -> Name
tvName (PlainTV name _) = name
tvName (KindedTV name _ _) = name

-- | Creates a 'TyVarBndr' for a plain variable without a kind annotation.
bindVar :: Name -> TyVarBndr Specificity
bindVar n = PlainTV n SpecifiedSpec

#else

-- | Fetches the 'Name' of a 'TyVarBndr'.
tvName :: TyVarBndr -> Name
tvName (PlainTV name) = name
tvName (KindedTV name _) = name

-- | Creates a 'TyVarBndr' for a plain variable without a kind annotation.
bindVar :: Name -> TyVarBndr
bindVar = PlainTV

#endif

-- | Gets the unapplied top-level name from a type application.
unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

-- | Substitutes a 'Type' for all occurrences of the given 'Name'.
substTypeVar :: Name -> Type -> Type -> Type
substTypeVar n t = substTypeVars [(n, t)]

-- | Makes variable substitutions from the given table.
substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

-- | Splits a type application into a top-level constructor and a list of its
-- type arguments.
splitTypeApp :: Type -> Maybe (Name, [Type])
splitTypeApp (ConT name) = Just (name, [])
splitTypeApp (AppT a b) = fmap (++ [b]) <$> splitTypeApp a
splitTypeApp _ = Nothing

-- | Splits a function type into a list of bound type vars, context, parameter
-- types, and return value type.
splitType :: Type -> ([Name], Cxt, [Type], Type)
splitType (ForallT tv cx b) =
  let (tvs, cxs, params, retval) = splitType b
   in (map tvName tv ++ tvs, cx ++ cxs, params, retval)
splitType (AppT (AppT ArrowT a) b) =
  let (tvs, cx, params, retval) = splitType b in (tvs, cx, a : params, retval)
splitType r = ([], [], [], r)

-- | Gets all free type variable 'Name's in the given 'Type'.
freeTypeVars :: Type -> [Name]
freeTypeVars = everythingWithContext [] (++) (mkQ ([],) go)
  where
    go (VarT v) bound
      | v `elem` bound = ([], bound)
      | otherwise = ([v], bound)
    go (ForallT vs _ _) bound = ([], map tvName vs ++ bound)
    go _ bound = ([], bound)

-- | Produces a 'CxtQ' that gives all given variable 'Name's all of the given
-- class 'Type's.
constrainVars :: [TypeQ] -> [Name] -> CxtQ
constrainVars cs vs = sequence [appT c (varT v) | c <- cs, v <- vs]

-- | Culls the given binders and constraints to choose only those that apply to
-- free variables in the given type.
relevantContext :: Type -> ([Name], Cxt) -> ([Name], Cxt)
relevantContext ty (tvs, cx) = (filter needsTv tvs, filteredCx)
  where
    filteredCx = filter (any (`elem` freeTypeVars ty) . freeTypeVars) cx
    needsTv v = any ((v `elem`) . freeTypeVars) (ty : filteredCx)

-- | Attempts to unify the given types by constructing a table of substitutions
-- for the variables of the left type that obtain the right one.
unifyTypes :: Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypes = unifyTypesWith []

-- | Unify types, but starting with a table of substitutions.
unifyTypesWith :: [(Name, Type)] -> Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypesWith tbl (VarT v) t2
  | Just t1 <- lookup v tbl = unifyTypesWith tbl t1 t2
  | otherwise = return (Just ((v, t2) : tbl))
unifyTypesWith tbl (ConT a) (ConT b) | a == b = return (Just tbl)
unifyTypesWith tbl a b = do
  mbA <- replaceSyn a
  mbB <- replaceSyn b
  case (mbA, mbB) of
    (Nothing, Nothing) -> unifyWithin tbl a b
    _ -> unifyTypesWith tbl (fromMaybe a mbA) (fromMaybe b mbB)
  where
    replaceSyn :: Type -> Q (Maybe Type)
    replaceSyn (ConT n) = do
      info <- reify n
      case info of
        TyConI (TySynD _ [] t) -> return (Just t)
        _ -> return Nothing
    replaceSyn _ = return Nothing

-- Unifies the types that occur within the arguments, starting with a table of
-- substitutions.
unifyWithin ::
  (Data a, Data b) => [(Name, Type)] -> a -> b -> Q (Maybe [(Name, Type)])
unifyWithin tbl a b
  | toConstr a == toConstr b =
    compose (gzipWithQ (\a' b' tbl' -> unify tbl' a' b') a b) tbl
  | otherwise = return Nothing
  where
    unify ::
      (Data a, Data b) => [(Name, Type)] -> a -> b -> Q (Maybe [(Name, Type)])
    unify tbl' a' b' = do
      case (cast a', cast b') of
        (Just a'', Just b'') -> unifyTypesWith tbl' a'' b''
        _ -> unifyWithin tbl' a' b'

    compose :: Monad m => [t -> m (Maybe t)] -> t -> m (Maybe t)
    compose [] x = return (Just x)
    compose (f : fs) x = do
      y <- f x
      case y of
        Just y' -> compose fs y'
        _ -> return Nothing

-- | Removes all module names from 'Name's in the given value, so that it will
-- pretty-print more cleanly.
removeModNames :: Data a => a -> a
removeModNames = everywhere (mkT unMod)
  where
    unMod NameG {} = NameS
    unMod other = other

-- | Determines if there is a polytype nested anywhere in the given type.
-- Top-level quantification doesn't count.
hasNestedPolyType :: Type -> Bool
hasNestedPolyType (ForallT _ _ t) = hasPolyType t
hasNestedPolyType t = hasPolyType t

-- | Determines if this is a polytype, including top-level quantification.
hasPolyType :: Type -> Bool
hasPolyType = everything (||) (mkQ False isPolyType)
  where
    isPolyType (ForallT tvs _ _) = not (null tvs)
    isPolyType _ = False

-- | Attempts to produce sufficient constraints for the given 'Type' to be an
-- instance of the given class 'Name'.
resolveInstance :: Name -> [Type] -> Q (Maybe Cxt)
resolveInstance cls args = do
  decs <- reifyInstances cls args
  results <- catMaybes <$> traverse (tryInstance args) decs
  case results of
    [cx] -> pure (Just cx)
    _ -> return Nothing
  where
    tryInstance :: [Type] -> InstanceDec -> Q (Maybe Cxt)
    tryInstance actualArgs (InstanceD _ cx instType _) =
      case splitTypeApp instType of
        Just (cls', instArgs)
          | cls' == cls ->
            unifyWithin [] instArgs actualArgs >>= \case
              Just tbl -> simplifyContext (substTypeVars tbl <$> cx)
              Nothing -> return Nothing
        _ -> return Nothing
    tryInstance _ _ = return Nothing

-- | Attempts to produce sufficient constraints for the given 'Type' to be a
-- satisfied constraint.  The type should be a class applied to its type
-- parameters.
--
-- Unlike 'simplifyContext', this function always resolves the top-level
-- constraint, and returns 'Nothing' if it cannot do so.
resolveInstanceType :: Type -> Q (Maybe Cxt)
resolveInstanceType t =
  maybe (pure Nothing) (uncurry resolveInstance) (splitTypeApp t)

-- | Simplifies a context with complex types (requiring FlexibleContexts) to try
-- to obtain one with all constraints applied to variables.
--
-- Should return Nothing if and only if the simplified contraint is
-- unsatisfiable, which is the case if and only if it contains a component with
-- no type variables.
simplifyContext :: Cxt -> Q (Maybe Cxt)
simplifyContext preds
  | all isVarApp preds = return (Just preds)
  | otherwise = do
    let simplifyPred t = fromMaybe [t] <$> resolveInstanceType t
    components <- concatMapM simplifyPred preds
    if any (null . freeTypeVars) components
      then return Nothing
      else return (Just (nub components))
  where
    isVarApp (ConT _) = True
    isVarApp (AppT t (VarT _)) | isVarApp t = True
    isVarApp _ = False

-- | Remove instance context from a method.
--
-- Some GHC versions report class members including the instance context (for
-- example, @show :: Show a => a -> String@, instead of @show :: a -> String@).
-- This looks for the instance context, and substitutes if needed to eliminate
-- it.
localizeMember :: Type -> Name -> Type -> Q Type
localizeMember instTy m t@(ForallT tvs cx ty) = do
  let fullConstraint = AppT instTy (VarT m)
  let unifyLeft (c, cs) = fmap (,cs) <$> unifyTypes c fullConstraint
  results <- mapMaybeM unifyLeft (choices cx)
  case results of
    ((tbl, remainingCx) : _) -> do
      let cx' = substTypeVars tbl <$> remainingCx
          ty' = substTypeVars tbl ty
          (tvs', cx'') =
            relevantContext
              ty'
              ((tvName <$> tvs) \\ (fst <$> tbl), cx')
          t'
            | null tvs' && null cx'' = ty'
            | otherwise = ForallT (bindVar <$> tvs') cx'' ty'
      return t'
    _ -> return t
localizeMember _ _ t = return t
