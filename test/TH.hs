{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TH where

import Data.Default
import Data.Dynamic
import Data.Kind
import Data.Typeable
import HMock
import HMock.Mockable
import HMock.TH

class MonadFoo1 m where
  foo1 :: String -> m ()

makeMockable [t|MonadFoo1|]

class MonadFoo2 a m where
  foo2 :: a -> m ()

makeMockable [t| MonadFoo2 Int |]

{-
instance Typeable a => Mockable (MonadFoo2 a) where
  data Action (MonadFoo2 a) :: Type -> Type where
    Foo2 :: a -> Action (MonadFoo2 a) ()
  data Matcher (MonadFoo2 a) :: Type -> Type where
    Foo2_ :: Predicate a -> Matcher (MonadFoo2 a) ()
  showAction (Foo2 a_aHW45) =
    unwords ("foo2" : ["_ :: a"])
  showMatcher (Just (Foo2 a_aHW48)) (Foo2_ p_aHW4a) =
    unwords ("foo2" : ["«" ++ (showPredicate p_aHW4a ++ "»")])
  showMatcher Nothing (Foo2_ p_aHW4a) =
    unwords ("foo2" : ["«" ++ (showPredicate p_aHW4a ++ "»")])
  match (Foo2_ p_aHW4b) (Foo2 a_aHW4c)
    | mismatches_aHW4d == 0 = FullMatch Refl
    | otherwise = PartialMatch Refl mismatches_aHW4d
    where
      mismatches_aHW4d = length (filter not [accept p_aHW4b a_aHW4c])

instance (Typeable a, Eq a, Show a) => ExactMockable (MonadFoo2 a) where
  exactly (Foo2 a_aHW4e) = Foo2_ (eq_ a_aHW4e)

instance
  (Typeable a, Typeable m_aHW4f, Monad m_aHW4f) =>
  MonadFoo2 a (MockT m_aHW4f)
  where
  foo2 a_aHW4g = mockMethod (Foo2 a_aHW4g)
-}

class MonadFoo3 m where
  foo3 :: Enum a => String -> a -> b -> m ()

makeMockable [t|MonadFoo3|]
