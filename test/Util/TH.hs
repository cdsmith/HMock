module Util.TH where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State
import Language.Haskell.TH
import Data.List (foldl')

deriveRecursive :: Maybe DerivStrategy -> Name -> Name -> Q [Dec]
deriveRecursive strat cls ty = evalStateT (concatMapM defineIfNeeded [ty]) []
  where
    defineIfNeeded :: Name -> StateT [Name] Q [Dec]
    defineIfNeeded t = do
      done <- gets (t `elem`)
      if done then return [] else modify (t :) >> defineInstance t

    defineInstance :: Name -> StateT [Name] Q [Dec]
    defineInstance t = do
      info <- lift (reify t)
      case info of
        TyConI (DataD _ n vs _ cons _)
          | n == t -> defineTypeAndCons t (length vs) cons
        TyConI (NewtypeD _ n vs _ con _)
          | n == t -> defineTypeAndCons t (length vs) [con]
        TyConI (TySynD _ _ t') -> concatMapM defineIfNeeded (typeToCons t')
        _ -> return []

    defineTypeAndCons :: Name -> Int -> [Con] -> StateT [Name] Q [Dec]
    defineTypeAndCons t nvars cons = do
      vs <- replicateM nvars (lift (newName "v"))
      let fullType = foldl' (\t' v -> AppT t' (VarT v)) (ConT t) vs
      let cx = AppT (ConT cls) . VarT <$> vs
      hasInstance <- lift (isInstance cls [fullType])
      if hasInstance
        then return []
        else do
          fieldDecls <-
            concatMapM defineIfNeeded $
              concatMap typeToCons $ concatMap conFieldTypes cons
          return
            ( StandaloneDerivD strat cx (AppT (ConT cls) fullType) :
              fieldDecls
            )

    conFieldTypes :: Con -> [Type]
    conFieldTypes (NormalC _ bts) = snd <$> bts
    conFieldTypes (RecC _ vbts) = (\(_, _, t) -> t) <$> vbts
    conFieldTypes (InfixC t1 _ t2) = [snd t1, snd t2]
    conFieldTypes (ForallC _ _ con) = conFieldTypes con
    conFieldTypes (GadtC _ bts _) = snd <$> bts
    conFieldTypes (RecGadtC _ vbts _) = (\(_, _, t) -> t) <$> vbts

    typeToCons :: Type -> [Name]
    typeToCons (ConT c) = [c]
    typeToCons (AppT ListT t) = typeToCons t
    typeToCons (AppT t1 t2) = typeToCons t1 ++ typeToCons t2
    typeToCons _ = []
