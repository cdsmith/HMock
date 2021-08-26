{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A type class and corresponding Template Haskell code for optional Show
-- instances.  The implementation here is based heavily on the code at
-- <https://github.com/mikeizbicki/ifcxt>, by Mike Izbicki, but is reproduced
-- here for release management reasons.
--
-- TODO: Recursively define instances for classes in contexts.
module Test.HMock.Internal.IfCxt where

import Control.Monad (forM, unless)
import Data.Proxy (Proxy (..))
import Language.Haskell.TH

class IfCxt cxt where
  ifCxt :: proxy cxt -> (cxt => a) -> a -> a

instance {-# OVERLAPPABLE #-} IfCxt cxt where ifCxt _ _ f = f

mkIfCxtInstances :: Name -> Q [Dec]
mkIfCxtInstances name = do
  scopedTypeVars <- isExtEnabled ScopedTypeVariables
  unless scopedTypeVars $ fail "ScopedTypeVariables must be enabled"

  ifCxtInfo <- reify ''IfCxt
  let instancesOfIfCxt = case ifCxtInfo of
        ClassI _ instances -> do
          flip map instances $ \inst -> do
            case inst of
              InstanceD _ _ (AppT _ t) _ -> t
              _ -> error "mkIfCxtInstances: unexpected IfCxt instance"
        _ ->
          error $
            "mkIfCxtInstances: "
              ++ "reify ''IfCxt returned something other than a class"
      isInstanceOfIfCxt t = t `elem` instancesOfIfCxt

  info <- reify name
  case info of
    ClassI _ instances -> do
      fmap concat $
        forM instances $ \inst -> do
          case inst of
            InstanceD _ cx appt@(AppT _ t) _ ->
              return $
                if isInstanceOfIfCxt appt then [] else mkInstance cx t name
            _ -> error "mkIfCxtInstances: unexpected instance"
    _ -> fail $ show name ++ " is not a class name."

mkInstance :: Cxt -> Type -> Name -> [Dec]
mkInstance cx t name =
  [ InstanceD
      Nothing
      (map relaxCxt cx)
      (relaxCxt (AppT (ConT name) t))
      [ FunD
          'ifCxt
          [ Clause
              [ WildP,
                VarP (mkName "t"),
                if null cx then WildP else VarP (mkName "f")
              ]
              (NormalB (mkIfCxtFun cx))
              []
          ]
      ]
  ]

relaxCxt :: Type -> Type
relaxCxt t@(AppT (ConT c) _)
  | c == ''IfCxt = t
relaxCxt t = AppT (ConT ''IfCxt) t

mkIfCxtFun :: Cxt -> Exp
mkIfCxtFun [] = VarE (mkName "t")
mkIfCxtFun (c : cs) =
  AppE (AppE (AppE (VarE 'ifCxt) proxy) (mkIfCxtFun cs)) (VarE (mkName "f"))
  where
    proxy = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) c)

-- | A type class for values that may or may not have 'Show' instances.  If a
-- 'Show' instance is available, it can be used; otherwise, a default answer can
-- be provided.
class IfCxt (Show a) => OptionalShow a
instance IfCxt (Show a) => OptionalShow a

-- | Like 'show', but with a default answer to be used when there is no 'Show'
-- instance.
showOr :: forall a. OptionalShow a => String -> a -> String
showOr d x = ifShow x id d

-- | A conditional form based on whether the first argument has a 'Show'
-- instance.  If so, the second argument is a function on its 'String' form.  If
-- not, the third argument is used.
ifShow :: forall a b. OptionalShow a => a -> (String -> b) -> b -> b
ifShow x f = ifCxt (Proxy :: Proxy (Show a)) (f (show x))

-- | Derives instances of 'OptionalShow' for all 'Show' instances in scope.  If
-- you've declared your own types, this might improve error messages for tests
-- that refer to them.
deriveOptionalShow :: Q [Dec]
deriveOptionalShow = mkIfCxtInstances ''Show
