{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HMock.Internal.TH where

import Control.Monad
import Control.Monad.Extra
import Data.Char
import Data.Generics
import Data.Maybe
import HMock.Internal.Core
import HMock.Internal.Predicates
import Language.Haskell.TH hiding (match)

unappliedName :: Type -> Maybe Name
unappliedName (AppT a _) = unappliedName a
unappliedName (ConT a) = Just a
unappliedName _ = Nothing

withClass :: Type -> (Dec -> Q a) -> Q a
withClass t f = do
  case unappliedName t of
    Just cls -> do
      info <- reify cls
      case info of
        ClassI dec@ClassD {} _ -> f dec
        _ -> fail $ "Expected " ++ show cls ++ " to be a class, but it wasn't."
    _ -> fail "Expected a class, but got something else."

getClassVars :: Type -> Q [(Name, Type)]
getClassVars t = withClass t $
  \(ClassD _ _ binders _ _) -> return (fst $ matchVars t binders)
  where
    matchVars (ConT _) vs = ([], vs)
    matchVars (AppT a b) vs = case matchVars a vs of
      (tbl, v : vs') -> ((toName v, b) : tbl, vs')
      (tbl, []) -> (tbl, [])
    matchVars _ vs = ([], vs)

    toName :: TyVarBndr -> Name
    toName (PlainTV n) = n
    toName (KindedTV n _) = n

substTypeVars :: [(Name, Type)] -> Type -> Type
substTypeVars classVars = everywhere (mkT subst)
  where
    subst (VarT x) | Just t <- lookup x classVars = t
    subst t = t

getMembers :: Type -> Q [Dec]
getMembers t = withClass t $ \(ClassD _ _ _ _ members) -> return members

data Method = Method
  { methodName :: Name,
    methodArgs :: [Type],
    methodResult :: Type
  }

getMethods :: Type -> Q [Method]
getMethods cls = mapMaybe parseMethod <$> getMembers cls

parseMethod :: Dec -> Maybe Method
parseMethod (SigD name ty)
  | argsAndReturn <- splitType ty,
    AppT (VarT _) result <- last argsAndReturn =
    Just (Method name (init argsAndReturn) result)
  where
    splitType (AppT (AppT ArrowT a) b) = a : splitType b
    splitType r = [r]
parseMethod _ = Nothing

hasNiceFields :: Method -> Q Bool
hasNiceFields (Method _ args _) = allM isNiceField args
  where
    isNiceField ty = (&&) <$> isInstance ''Show [ty] <*> isInstance ''Eq [ty]

makeMockable :: Q Type -> Q [Dec]
makeMockable qt = (++) <$> deriveMockable qt <*> deriveForMockT qt

deriveMockable :: Q Type -> Q [Dec]
deriveMockable qt = do
  t <- qt
  methods <- getMethods t
  mockableDecs <-
    sequenceA
      [ defineActionType t methods,
        defineMatcherType t methods,
        defineShowAction methods,
        defineShowMatcher methods,
        defineMatch methods
      ]
  let mockableInst =
        [InstanceD Nothing [] (AppT (ConT ''Mockable) t) mockableDecs]

  exact <- allM hasNiceFields methods
  exactMockableInst <-
    if exact
      then do
        exactDecs <-
          sequenceA
            [ defineExactly methods
            ]
        return
          [InstanceD Nothing [] (AppT (ConT ''ExactMockable) t) exactDecs]
      else return []

  return (mockableInst ++ exactMockableInst)

defineActionType :: Type -> [Method] -> Q Dec
defineActionType t methods = do
  classVars <- getClassVars t
  a <- newName "a"
  let conDecs = actionConstructor t classVars <$> methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Action) t) (VarT a))
        Nothing
        conDecs
        []
    )

actionConstructor :: Type -> [(Name, Type)] -> Method -> Con
actionConstructor t classVars (Method name args result) =
  GadtC
    [methodToActionName name]
    (map ((s,) . substTypeVars classVars) args)
    target
  where
    target = AppT (AppT (ConT ''Action) t) (substTypeVars classVars result)
    s = Bang NoSourceUnpackedness NoSourceStrictness

methodToActionName :: Name -> Name
methodToActionName name = mkName (toUpper c : cs)
  where
    (c : cs) = nameBase name

defineMatcherType :: Type -> [Method] -> Q Dec
defineMatcherType t methods = do
  classVars <- getClassVars t
  a <- newName "a"
  let conDecs = matcherConstructor t classVars <$> methods
  return
    ( DataInstD
        []
        Nothing
        (AppT (AppT (ConT ''Matcher) t) (VarT a))
        Nothing
        conDecs
        []
    )

matcherConstructor :: Type -> [(Name, Type)] -> Method -> Con
matcherConstructor t classVars (Method name args result) =
  GadtC
    [methodToMatcherName name]
    ((s,) . AppT (ConT ''Predicate) . substTypeVars classVars <$> args)
    target
  where
    target = AppT (AppT (ConT ''Matcher) t) (substTypeVars classVars result)
    s = Bang NoSourceUnpackedness NoSourceStrictness

methodToMatcherName :: Name -> Name
methodToMatcherName name = mkName (toUpper c : cs ++ "_")
  where
    (c : cs) = nameBase name

defineShowAction :: [Method] -> Q Dec
defineShowAction methods = do
  clauses <- traverse showActionClause methods
  return (FunD 'showAction clauses)

showActionClause :: Method -> Q Clause
showActionClause (Method name args _) = do
  argVars <- replicateM (length args) (newName "p")
  printedArgs <- traverse showArg (zip args argVars)
  let body =
        NormalB
          ( AppE
              (VarE 'unwords)
              (ListE (LitE (StringL (nameBase name)) : printedArgs))
          )
  return (Clause [ConP (methodToActionName name) (VarP <$> argVars)] body [])
  where
    showArg (ty, var) = do
      showable <- isInstance ''Show [ty]
      return $
        if showable then AppE (VarE 'show) (VarE var) else LitE (StringL "_")

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

deriveForMockT :: Q Type -> Q [Dec]
deriveForMockT qt = do
  t <- qt
  maybeMethods <- traverse parseMethod <$> getMembers t
  case maybeMethods of
    Nothing ->
      fail $
        "Cannot derive MockT because " ++ show t ++ " is too complex."
    Just methods -> do
      m <- newName "m"
      decs <- traverse mockMethodImpl methods
      return
        [ InstanceD
            Nothing
            [AppT (ConT ''Typeable) (VarT m), AppT (ConT ''Monad) (VarT m)]
            (AppT t (AppT (ConT ''MockT) (VarT m)))
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
