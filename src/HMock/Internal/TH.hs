{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HMock.Internal.TH where

import Control.Monad
import Data.Char
import Data.Typeable
import HMock.Internal.Core
import HMock.Internal.Predicates
import Language.Haskell.TH hiding (Match, match)

makeMockable :: Name -> Q [Dec]
makeMockable cls = do
  methods <- getMethods cls
  sequenceA
    [ deriveMockable cls methods,
      deriveForMockT cls methods
    ]

getMethods :: Name -> Q [Dec]
getMethods cls = do
  info <- reify cls
  case info of
    ClassI (ClassD _ _ _ _ methods) _ -> return methods
    _ -> error $ "parameter wasn't a class: " ++ show cls

deriveMockable :: Name -> [Dec] -> Q Dec
deriveMockable cls methods = do
  decs <-
    sequenceA
      [ defineActionType cls methods,
        defineMatchType cls methods,
        defineShowAction methods,
        defineShowMatch methods,
        defineExactly methods,
        defineMatch methods
      ]
  return (InstanceD Nothing [] (AppT (ConT ''Mockable) (ConT cls)) decs)

fnArgsAndReturn :: Type -> [Type]
fnArgsAndReturn (AppT (AppT ArrowT a) b) = a : fnArgsAndReturn b
fnArgsAndReturn r = [r]

defineActionType :: Name -> [Dec] -> Q Dec
defineActionType cls methods = do
  a <- newName "a"
  conDecs <- traverse (actionConstructor cls) methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Action) (ConT cls)) (VarT a))
        Nothing
        conDecs
        []
    )

actionConstructor :: Name -> Dec -> Q Con
actionConstructor cls (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
      AppT (VarT _) ret = last argsAndReturn
      result = AppT (AppT (ConT ''Action) (ConT cls)) ret
  return
    (GadtC [methodToActionName name] (map (s,) (init argsAndReturn)) result)
  where
    s = Bang NoSourceUnpackedness NoSourceStrictness
actionConstructor _ _ = error "bad method type"

methodToActionName :: Name -> Name
methodToActionName name = mkName (toUpper c : cs)
  where
    (c : cs) = nameBase name

defineMatchType :: Name -> [Dec] -> Q Dec
defineMatchType cls methods = do
  a <- newName "a"
  conDecs <- traverse (matchConstructor cls) methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Match) (ConT cls)) (VarT a))
        Nothing
        conDecs
        []
    )

methodToMatchName :: Name -> Name
methodToMatchName name = mkName (toUpper c : cs ++ "_")
  where
    (c : cs) = nameBase name

matchConstructor :: Name -> Dec -> Q Con
matchConstructor cls (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
      AppT (VarT _) ret = last argsAndReturn
      result = AppT (AppT (ConT ''Match) (ConT cls)) ret
  return
    ( GadtC
        [methodToMatchName name]
        (map ((s,) . AppT (ConT ''Predicate)) (init argsAndReturn))
        result
    )
  where
    s = Bang NoSourceUnpackedness NoSourceStrictness
matchConstructor _ _ = error "bad method type"

defineShowAction :: [Dec] -> Q Dec
defineShowAction methods = do
  clauses <- traverse showActionClause methods
  return (FunD 'showAction clauses)

showActionClause :: Dec -> Q Clause
showActionClause (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
  argVars <-
    traverse
      (\(_, i) -> newName ("p" ++ show i))
      (zip (init argsAndReturn) [1 :: Int ..])
  let body =
        NormalB
          ( AppE
              (VarE 'unwords)
              ( ListE
                  ( LitE (StringL (nameBase name)) :
                    map (AppE (VarE 'show) . VarE) argVars
                  )
              )
          )
  return (Clause [ConP (methodToActionName name) (VarP <$> argVars)] body [])
showActionClause _ = error "bad method type"

defineShowMatch :: [Dec] -> Q Dec
defineShowMatch methods = do
  clauses <- traverse showMatchClause methods
  return (FunD 'showMatch clauses)

showMatchClause :: Dec -> Q Clause
showMatchClause (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
  argVars <-
    traverse
      (\(_, i) -> newName ("p" ++ show i))
      (zip (init argsAndReturn) [1 :: Int ..])
  printedArgs <- traverse showArg argVars
  let body =
        NormalB
          ( AppE
              (VarE 'unwords)
              ( ListE
                  ( LitE (StringL (nameBase name)) :
                    printedArgs
                  )
              )
          )
  return (Clause [ConP (methodToMatchName name) (VarP <$> argVars)] body [])
  where
    showArg a = [|"\171" ++ showPredicate $(return (VarE a)) ++ "\187"|]
showMatchClause _ = error "bad method type"

defineExactly :: [Dec] -> Q Dec
defineExactly methods = do
  clauses <- traverse exactlyClause methods
  return (FunD 'exactly clauses)

exactlyClause :: Dec -> Q Clause
exactlyClause (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
  argVars <-
    traverse
      (\(_, i) -> newName ("p" ++ show i))
      (zip (init argsAndReturn) [1 :: Int ..])
  return
    ( Clause
        [ConP (methodToActionName name) (VarP <$> argVars)]
        (NormalB (makeBody (ConE (methodToMatchName name)) argVars))
        []
    )
  where
    makeBody e [] = e
    makeBody e (v : vs) = makeBody (AppE e (AppE (VarE 'eq_) (VarE v))) vs
exactlyClause _ = error "bad method type"

defineMatch :: [Dec] -> Q Dec
defineMatch methods = do
  let fallthrough = Clause [WildP, WildP] (NormalB (ConE 'NoMatch)) []
  clauses <- (++ [fallthrough]) <$> traverse matchClause methods
  return (FunD 'match clauses)

matchClause :: Dec -> Q Clause
matchClause (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
  vars <- forM (zip (init argsAndReturn) [1 :: Int ..]) $ \(_, i) ->
    (,) <$> newName ("p" ++ show i) <*> newName ("a" ++ show i)
  mismatchVar <- newName "mismatches"
  matches <-
    traverse
      (\(p, a) -> [|accept $(return (VarE p)) $(return (VarE a))|])
      vars

  return
    ( Clause
        [ ConP (methodToMatchName name) (VarP . fst <$> vars),
          ConP (methodToActionName name) (VarP . snd <$> vars)
        ]
        ( GuardedB
            [ ( NormalG
                  ( InfixE
                      (Just (VarE mismatchVar))
                      (VarE '(==))
                      (Just (LitE (IntegerL 0)))
                  ),
                AppE (ConE 'FullMatch) (UnboundVarE 'Refl)
              ),
              ( NormalG (VarE 'otherwise),
                AppE (ConE 'PartialMatch) (VarE mismatchVar)
              )
            ]
        )
        [ ValD
            (VarP mismatchVar)
            ( NormalB
                ( InfixE
                    (Just (VarE 'length))
                    (VarE '($))
                    ( Just
                        ( AppE
                            ( AppE
                                (VarE 'filter)
                                (VarE 'not)
                            )
                            (ListE matches)
                        )
                    )
                )
            )
            []
        ]
    )
matchClause _ = error "bad method type"

deriveForMockT :: Name -> [Dec] -> Q Dec
deriveForMockT cls methods = do
  m <- newName "m"
  decs <- traverse mockMethod methods
  return
    ( InstanceD
        Nothing
        [AppT (ConT ''Typeable) (VarT m), AppT (ConT ''Monad) (VarT m)]
        (AppT (ConT cls) (AppT (ConT ''MockT) (VarT m)))
        decs
    )

mockMethod :: Dec -> Q Dec
mockMethod (SigD name ty) = do
  let argsAndReturn = fnArgsAndReturn ty
  argVars <-
    traverse
      (\(_, i) -> newName ("p" ++ show i))
      (zip (init argsAndReturn) [1 :: Int ..])
  return
    ( FunD
        name
        [ Clause
            (VarP <$> argVars)
            ( NormalB
                ( AppE
                    (VarE 'mockAction)
                    (actionExp (UnboundVarE (methodToActionName name)) argVars)
                )
            )
            []
        ]
    )
  where
    actionExp e [] = e
    actionExp e (v : vs) = actionExp (AppE e (VarE v)) vs
mockMethod _ = error "bad method type"
