{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HMock.Internal.TH where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Typeable
import HMock.Internal.Core
import HMock.Internal.Predicates
import Language.Haskell.TH hiding (match)

data Method = Method
  { methodName :: Name,
    methodArgs :: [Type],
    methodResult :: Type
  }

getMethods :: Name -> Q [Method]
getMethods cls = mapMaybe parseMethod <$> getMembers cls

getMembers :: Name -> Q [Dec]
getMembers cls = do
  info <- reify cls
  case info of
    ClassI (ClassD _ _ _ _ members) _ -> return members
    _ -> fail $ "Expected " ++ show cls ++ " to be a class, but it wasn't."

parseMethod :: Dec -> Maybe Method
parseMethod (SigD name ty)
  | argsAndReturn <- fnArgsAndReturn ty,
    AppT (VarT _) result <- last argsAndReturn =
    Just (Method name (init argsAndReturn) result)
parseMethod _ = Nothing

makeMockable :: Name -> Q [Dec]
makeMockable cls = (++) <$> deriveMockable cls <*> deriveForMockT cls

deriveMockable :: Name -> Q [Dec]
deriveMockable cls = do
  methods <- getMethods cls
  decs <-
    sequenceA
      [ defineActionType cls methods,
        defineMatcherType cls methods,
        defineShowAction methods,
        defineShowMatcher methods,
        defineExactly methods,
        defineMatch methods
      ]
  return [InstanceD Nothing [] (AppT (ConT ''Mockable) (ConT cls)) decs]

fnArgsAndReturn :: Type -> [Type]
fnArgsAndReturn (AppT (AppT ArrowT a) b) = a : fnArgsAndReturn b
fnArgsAndReturn r = [r]

defineActionType :: Name -> [Method] -> Q Dec
defineActionType cls methods = do
  a <- newName "a"
  let conDecs = actionConstructor cls <$> methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Action) (ConT cls)) (VarT a))
        Nothing
        conDecs
        []
    )

actionConstructor :: Name -> Method -> Con
actionConstructor cls (Method name args result) =
  GadtC [methodToActionName name] (map (s,) args) target
  where
    target = AppT (AppT (ConT ''Action) (ConT cls)) result
    s = Bang NoSourceUnpackedness NoSourceStrictness

methodToActionName :: Name -> Name
methodToActionName name = mkName (toUpper c : cs)
  where
    (c : cs) = nameBase name

defineMatcherType :: Name -> [Method] -> Q Dec
defineMatcherType cls methods = do
  a <- newName "a"
  let conDecs = matcherConstructor cls <$> methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Matcher) (ConT cls)) (VarT a))
        Nothing
        conDecs
        []
    )

methodToMatcherName :: Name -> Name
methodToMatcherName name = mkName (toUpper c : cs ++ "_")
  where
    (c : cs) = nameBase name

matcherConstructor :: Name -> Method -> Con
matcherConstructor cls (Method name args result) =
  GadtC
    [methodToMatcherName name]
    ((s,) . AppT (ConT ''Predicate) <$> args)
    target
  where
    target = AppT (AppT (ConT ''Matcher) (ConT cls)) result
    s = Bang NoSourceUnpackedness NoSourceStrictness

defineShowAction :: [Method] -> Q Dec
defineShowAction methods = do
  clauses <- traverse showActionClause methods
  return (FunD 'showAction clauses)

showActionClause :: Method -> Q Clause
showActionClause (Method name args _) = do
  argVars <- replicateM (length args) (newName "p")
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

defineShowMatcher :: [Method] -> Q Dec
defineShowMatcher methods = do
  clauses <- traverse showMatcherClause methods
  return (FunD 'showMatcher clauses)

showMatcherClause :: Method -> Q Clause
showMatcherClause (Method name args _) = do
  argVars <- replicateM (length args) (newName "p")
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
  return (Clause [ConP (methodToMatcherName name) (VarP <$> argVars)] body [])
  where
    showArg a = [|"«" ++ showPredicate $(varE a) ++ "»"|]

defineExactly :: [Method] -> Q Dec
defineExactly methods = do
  clauses <- traverse exactlyClause methods
  return (FunD 'exactly clauses)

exactlyClause :: Method -> Q Clause
exactlyClause (Method name args _) = do
  argVars <- replicateM (length args) (newName "p")
  return
    ( Clause
        [ConP (methodToActionName name) (VarP <$> argVars)]
        (NormalB (makeBody (ConE (methodToMatcherName name)) argVars))
        []
    )
  where
    makeBody e [] = e
    makeBody e (v : vs) = makeBody (AppE e (AppE (VarE 'eq_) (VarE v))) vs

defineMatch :: [Method] -> Q Dec
defineMatch methods = do
  let fallthrough = Clause [WildP, WildP] (NormalB (ConE 'NoMatch)) []
  clauses <- (++ [fallthrough]) <$> traverse matchClause methods
  return (FunD 'match clauses)

matchClause :: Method -> Q Clause
matchClause (Method name args _) = do
  let n = length args
  vars <- zip <$> replicateM n (newName "p") <*> replicateM n (newName "a")
  mismatchVar <- newName "mismatches"
  matches <-
    traverse
      (\(p, a) -> [|accept $(return (VarE p)) $(return (VarE a))|])
      vars

  return
    ( Clause
        [ ConP (methodToMatcherName name) (VarP . fst <$> vars),
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

deriveForMockT :: Name -> Q [Dec]
deriveForMockT cls = do
  maybeMethods <- traverse parseMethod <$> getMembers cls
  case maybeMethods of
    Nothing ->
      fail $
        "Cannot derive MockT because " ++ nameBase cls ++ " is too complex."
    Just methods -> do
      m <- newName "m"
      decs <- traverse mockMethodImpl methods
      return
        [ InstanceD
            Nothing
            [AppT (ConT ''Typeable) (VarT m), AppT (ConT ''Monad) (VarT m)]
            (AppT (ConT cls) (AppT (ConT ''MockT) (VarT m)))
            decs
        ]

mockMethodImpl :: Method -> Q Dec
mockMethodImpl (Method name args _) = do
  argVars <- replicateM (length args) (newName "p")
  return
    ( FunD
        name
        [ Clause
            (VarP <$> argVars)
            ( NormalB
                ( AppE
                    (VarE 'mockMethod)
                    (actionExp (UnboundVarE (methodToActionName name)) argVars)
                )
            )
            []
        ]
    )
  where
    actionExp e [] = e
    actionExp e (v : vs) = actionExp (AppE e (VarE v)) vs
