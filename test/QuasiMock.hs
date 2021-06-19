{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module QuasiMock where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.Generics (Typeable, everything, mkQ)
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import Test.HMock
import Test.HMock.TH (deriveMockableBase)
import Util.TH (reifyInstancesStatic, reifyStatic)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

deriveMockableBase ''Quasi

-- Because not all methods of Quasi are mockable, the instance must be written
-- by hand.
instance (Typeable m, MonadFail m, MonadIO m) => Quasi (MockT m) where
  -- Mocks
  qReport b s = mockMethod (QReport b s)
  qLookupName b s = mockMethod (QLookupName b s)
  qReify n = mockDefaultlessMethod (QReify n)
  qReifyFixity n = mockMethod (QReifyFixity n)
  qReifyInstances n ts = mockMethod (QReifyInstances n ts)
  qReifyRoles n = mockMethod (QReifyRoles n)
  qReifyModule m = mockDefaultlessMethod (QReifyModule m)
  qReifyConStrictness n = mockMethod (QReifyConStrictness n)
  qLocation = mockDefaultlessMethod QLocation
  qAddDependentFile f = mockMethod (QAddDependentFile f)
  qAddTopDecls ds = mockMethod (QAddTopDecls ds)
  qAddModFinalizer f = mockMethod (QAddModFinalizer f)
  qAddCorePlugin s = mockMethod (QAddCorePlugin s)
  qPutQ a = mockMethod (QPutQ a)
  qIsExtEnabled e = mockDefaultlessMethod (QIsExtEnabled e)
  qExtsEnabled = mockMethod QExtsEnabled

#if MIN_VERSION_template_haskell(2, 14, 0)
  qAddTempFile s = mockMethod (QAddTempFile s)
  qAddForeignFilePath l s = mockMethod (QAddForeignFilePath l s)
#else
  qAddForeignFile l s = mockMethod (QAddForeignFile l s)
#endif

#if MIN_VERSION_template_haskell(2, 16, 0)
  qReifyType n = mockDefaultlessMethod (QReifyType n)
#endif

  -- Methods delegated to IO
  qNewName s = liftIO (qNewName s)

  -- Non-mockable methods that cannot be lifted to IO
  qGetQ = error "qGetQ"
  qRecover = error "qRecover"
  qReifyAnnotations = error "qReifyAnnotations"

functionType :: [Type] -> Bool
functionType = everything (||) (mkQ False isArrow)
  where
    isArrow ArrowT = True
    isArrow _ = False

instance Mockable Quasi where
  setupMockable _ = do
    expectAny $ QIsExtEnabled_ anything |-> True

    expectAny $ QReify ''String |-> $(reifyStatic ''String)
    expectAny $ QReify ''Char |-> $(reifyStatic ''Char)
    expectAny $ QReify ''Int |-> $(reifyStatic ''Int)
    expectAny $ QReify ''Bool |-> $(reifyStatic ''Bool)
    expectAny $ QReify ''Enum |-> $(reifyStatic ''Enum)
    expectAny $ QReify ''Monad |-> $(reifyStatic ''Monad)
    expectAny $
      QReifyInstances ''Show [ConT ''String]
        |-> $(reifyInstancesStatic ''Show [ConT ''String])
    expectAny $
      QReifyInstances ''Eq [ConT ''String]
        |-> $(reifyInstancesStatic ''Eq [ConT ''String])
    expectAny $
      QReifyInstances ''Show [ConT ''Char]
        |-> $(reifyInstancesStatic ''Show [ConT ''Char])
    expectAny $
      QReifyInstances ''Eq [ConT ''Char]
        |-> $(reifyInstancesStatic ''Eq [ConT ''Char])
    expectAny $
      QReifyInstances ''Show [ConT ''Int]
        |-> $(reifyInstancesStatic ''Show [ConT ''Int])
    expectAny $
      QReifyInstances ''Eq [ConT ''Int]
        |-> $(reifyInstancesStatic ''Eq [ConT ''Int])
    expectAny $
      QReifyInstances ''Show [ConT ''Bool]
        |-> $(reifyInstancesStatic ''Show [ConT ''Bool])
    expectAny $
      QReifyInstances ''Eq [ConT ''Bool]
        |-> $(reifyInstancesStatic ''Eq [ConT ''Bool])
    expectAny $
      QReifyInstances ''Default [TupleT 0]
        |-> $(reifyInstancesStatic ''Default [TupleT 0])
    expectAny $
      QReifyInstances ''Default [ConT ''String]
        |-> $(reifyInstancesStatic ''Default [ConT ''String])
    expectAny $
      QReifyInstances ''Default [ConT ''Int]
        |-> $(reifyInstancesStatic ''Default [ConT ''Int])
    expectAny $
      QReifyInstances ''Default [AppT (ConT ''Maybe) (ConT ''Bool)]
        |-> $(reifyInstancesStatic ''Default [AppT (ConT ''Maybe) (ConT ''Bool)])
    expectAny $
      QReifyInstances_ (eq ''Show) (is functionType) |-> []
    expectAny $
      QReifyInstances_ (eq ''Eq) (is functionType) |-> []

    expectAny $
      QReifyInstances_ (eq ''Show) (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Show [AppT ListT (VarT (mkName "a_0"))])
    expectAny $
      QReifyInstances_ (eq ''Eq) (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Eq [AppT ListT (VarT (mkName "a_0"))])