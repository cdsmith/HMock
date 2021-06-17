{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Test.HMock.Internal.TH.Util
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
    simplifyContext,
    localizeMember,
  )
where

import Control.Monad.Extra (mapMaybeM)
import Data.Generics
import Data.List ((\\))
import Data.Maybe (catMaybes, fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (NameFlavour (..))
import Test.HMock.Internal.Util (choices)

#if MIN_VERSION_template_haskell(2,17,0)

tvName :: TyVarBndr flag -> Name
tvName (PlainTV name _) = name
tvName (KindedTV name _ _) = name

bindVar :: Name -> TyVarBndr Specificity
bindVar n = PlainTV n SpecifiedSpec

#else

tvName :: TyVarBndr -> Name
tvName (PlainTV name) = name
tvName (KindedTV name _) = name

bindVar :: Name -> TyVarBndr
bindVar = PlainTV

#endif

unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

substTypeVar :: Name -> Type -> Type -> Type
substTypeVar n t = substTypeVars [(n, t)]

substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

splitType :: Type -> ([Name], Cxt, [Type])
splitType (ForallT tv cx b) =
  let (tvs, cxs, parts) = splitType b
   in (map tvName tv ++ tvs, cx ++ cxs, parts)
splitType (AppT (AppT ArrowT a) b) =
  let (tvs, cx, parts) = splitType b in (tvs, cx, a : parts)
splitType r = ([], [], [r])

freeTypeVars :: Type -> [Name]
freeTypeVars = everythingWithContext [] (++) (mkQ ([],) go)
  where
    go (VarT v) bound
      | v `elem` bound = ([], bound)
      | otherwise = ([v], bound)
    go (ForallT vs _ _) bound = ([], map tvName vs ++ bound)
    go _ bound = ([], bound)

constrainVars :: [TypeQ] -> [Name] -> CxtQ
constrainVars cs vs = sequence [appT c (varT v) | c <- cs, v <- vs]

relevantContext :: Type -> ([Name], Cxt) -> ([Name], Cxt)
relevantContext ty (tvs, cx) = (filter needsTv tvs, filteredCx)
  where
    filteredCx = filter (any (`elem` freeTypeVars ty) . freeTypeVars) cx
    needsTv v = any ((v `elem`) . freeTypeVars) (ty : filteredCx)

unifyTypes :: Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypes = unifyTypesWith []

unifyTypesWith :: [(Name, Type)] -> Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypesWith tbl (VarT v) t2
  | Just t1 <- lookup v tbl = unifyTypesWith tbl t1 t2
  | otherwise = return (Just ((v, t2) : tbl))
unifyTypesWith tbl (ConT a) (ConT b) | a == b = return (Just tbl)
unifyTypesWith tbl a b = do
  mbA <- replaceSyn a
  mbB <- replaceSyn b
  case (mbA, mbB) of
    (Nothing, Nothing) -> unifyGen tbl a b
    _ -> unifyTypesWith tbl (fromMaybe a mbA) (fromMaybe b mbB)

replaceSyn :: Type -> Q (Maybe Type)
replaceSyn (ConT n) = do
  info <- reify n
  case info of
    TyConI (TySynD _ [] t) -> return (Just t)
    _ -> return Nothing
replaceSyn _ = return Nothing

unifyGen ::
  (Data a, Data b) => [(Name, Type)] -> a -> b -> Q (Maybe [(Name, Type)])
unifyGen tbl a b
  | toConstr a == toConstr b =
    compose (gzipWithQ (\a' b' tbl' -> unify tbl' a' b') a b) tbl
  | otherwise = return Nothing

unify ::
  (Data a, Data b) => [(Name, Type)] -> a -> b -> Q (Maybe [(Name, Type)])
unify tbl a b = do
  case (cast a, cast b) of
    (Just a', Just b') -> unifyTypesWith tbl a' b'
    _ -> unifyGen tbl a b

compose :: Monad m => [t -> m (Maybe t)] -> t -> m (Maybe t)
compose [] x = return (Just x)
compose (f : fs) x = do
  y <- f x
  case y of
    Just y' -> compose fs y'
    _ -> return Nothing

removeModNames :: Data a => a -> a
removeModNames = everywhere (mkT unMod)
  where
    unMod NameG {} = NameS
    unMod other = other

hasNestedPolyType :: Type -> Bool
hasNestedPolyType (ForallT _ _ t) = hasPolyType t
hasNestedPolyType t = hasPolyType t

hasPolyType :: Type -> Bool
hasPolyType = everything (||) (mkQ False isPolyType)
  where
    isPolyType (ForallT tvs _ _) = not (null tvs)
    isPolyType _ = False

resolveInstance :: Name -> Type -> Q (Maybe Cxt)
resolveInstance cls t@(VarT _) = return (Just [AppT (ConT cls) t])
resolveInstance cls t = do
  decs <- reifyInstances cls [t]
  result <- traverse (tryInstance t) decs
  case catMaybes result of
    [cx] -> return (Just (filter (not . null . freeTypeVars) cx))
    _ -> return Nothing
  where
    tryInstance :: Type -> InstanceDec -> Q (Maybe Cxt)
    tryInstance actualTy (InstanceD _ cx (AppT (ConT cls') genTy) _)
      | cls' == cls =
        unifyTypes genTy actualTy >>= \case
          Just tbl ->
            let cx' = substTypeVars tbl <$> cx
             in fmap concat . sequence <$> mapM resolveInstanceType cx'
          Nothing -> return Nothing
    tryInstance _ _ = return Nothing

    resolveInstanceType :: Type -> Q (Maybe Cxt)
    resolveInstanceType (AppT (ConT cls') t') = resolveInstance cls' t'
    resolveInstanceType _ = return Nothing

simplifyContext :: Cxt -> Q (Maybe Cxt)
simplifyContext (AppT (ConT cls) t : preds) = resolveInstance cls t >>= \case
  Just cxt' -> fmap (cxt' ++) <$> simplifyContext preds
  Nothing -> return Nothing
simplifyContext (otherPred : preds) = fmap (otherPred :) <$> simplifyContext preds
simplifyContext [] = return (Just [])

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
