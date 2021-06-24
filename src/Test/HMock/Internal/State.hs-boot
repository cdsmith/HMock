{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module Test.HMock.Internal.State where

import Data.Kind (Type)

type role MockT nominal nominal

data MockT (m :: Type -> Type) (a :: Type)

instance Monad m => Monad (MockT m)

type role MockSetup nominal nominal

data MockSetup (m :: Type -> Type) (a :: Type)

instance Monad (MockSetup m)
