{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module Test.HMock.Internal.MockT where

import Data.Kind (Type)

type role MockT nominal nominal

data MockT (m :: Type -> Type) (a :: Type)

instance Monad m => Monad (MockT m)

type role MockSetupT nominal nominal

data MockSetupT (m :: Type -> Type) (a :: Type)

instance Monad (MockSetupT m)
