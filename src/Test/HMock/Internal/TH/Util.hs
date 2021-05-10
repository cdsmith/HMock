{-# LANGUAGE TupleSections #-}

module Test.HMock.Internal.TH.Util
  ( unappliedName,
    tvName,
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
  )
where

import Data.Generics
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (NameFlavour (..))

unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

tvName :: TyVarBndr -> Name
tvName (PlainTV name) = name
tvName (KindedTV name _) = name

substTypeVar :: Name -> Type -> Type -> Type
substTypeVar n t = substTypeVars [(n, t)]

substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

splitType :: Type -> ([TyVarBndr], Cxt, [Type])
splitType (ForallT tv cx b) =
  let (tvs, cxs, parts) = splitType b in (tv ++ tvs, cx ++ cxs, parts)
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

relevantContext :: Type -> ([TyVarBndr], Cxt) -> ([TyVarBndr], Cxt)
relevantContext ty (tvs, cx) =
  (filter (tvHasVar free) tvs, filter (cxtHasVar free) cx)
  where
    free = freeTypeVars ty
    tvHasVar vars tv = tvName tv `elem` vars
    cxtHasVar vars t = any (`elem` vars) (freeTypeVars t)

unifyTypes :: Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypes = unifyTypesWith []

unifyTypesWith :: [(Name, Type)] -> Type -> Type -> Q (Maybe [(Name, Type)])
unifyTypesWith tbl (VarT v) t2
  | Just t1 <- lookup v tbl = unifyTypesWith tbl t1 t2
  | otherwise = return (Just [(v, t2)])
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
    isPolyType ForallT {} = True
    isPolyType _ = False
