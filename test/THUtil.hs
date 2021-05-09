module THUtil where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State
import Language.Haskell.TH

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
        TyConI (DataD _ n [] _ cons _) | n == t -> defineTypeAndCons t cons
        TyConI (NewtypeD _ n [] _ con _) | n == t -> defineTypeAndCons t [con]
        TyConI (TySynD _ _ t') -> concatMapM defineIfNeeded (typeToCons t')
        _ -> return []

    defineTypeAndCons :: Name -> [Con] -> StateT [Name] Q [Dec]
    defineTypeAndCons t cons = do
      hasInstance <- lift (isInstance cls [ConT t])
      if hasInstance
        then return []
        else do
          fieldDecls <-
            concatMapM defineIfNeeded $
              concatMap typeToCons $ concatMap conFieldTypes cons
          return
            ( StandaloneDerivD strat [] (AppT (ConT cls) (ConT t)) :
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
