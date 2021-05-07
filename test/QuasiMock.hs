{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QuasiMock where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Generics (Typeable, everything, mkQ)
import HMock
import HMock.Mockable
import HMock.TH
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import THUtil

deriveMockable ''Quasi

-- Because not all methods of Quasi are mockable, the instance must be written
-- by hand.
instance (Typeable m, MonadFail m, MonadIO m) => Quasi (MockT m) where
  -- Mocks
  qReport b s = mockMethod (QReport b s)
  qLookupName b s = mockMethod (QLookupName b s)
  qReify n = mockMethod (QReify n)
  qReifyFixity n = mockMethod (QReifyFixity n)
  qReifyType n = mockMethod (QReifyType n)
  qReifyInstances n ts = mockMethod (QReifyInstances n ts)
  qReifyRoles n = mockMethod (QReifyRoles n)
  qReifyModule m = mockMethod (QReifyModule m)
  qReifyConStrictness n = mockMethod (QReifyConStrictness n)
  qLocation = mockMethod QLocation
  qAddDependentFile f = mockMethod (QAddDependentFile f)
  qAddTempFile s = mockMethod (QAddTempFile s)
  qAddTopDecls ds = mockMethod (QAddTopDecls ds)
  qAddForeignFilePath l s = mockMethod (QAddForeignFilePath l s)
  qAddModFinalizer f = mockMethod (QAddModFinalizer f)
  qAddCorePlugin s = mockMethod (QAddCorePlugin s)
  qPutQ a = mockMethod (QPutQ a)
  qIsExtEnabled e = mockMethod (QIsExtEnabled e)
  qExtsEnabled = mockMethod QExtsEnabled

  -- Methods delegated to IO
  qNewName s = liftIO (qNewName s)

  -- Non-mockable methods that cannot be lifted to IO
  qGetQ = error "qGetQ"
  qRecover = error "qRecover"
  qReifyAnnotations = error "qReifyAnnotations"

-- | Sets up some common default behaviors for the Quasi type class.
setupQuasi :: (Typeable m, MonadIO m, MonadFail m) => MockT m ()
setupQuasi = do
  mock $ whenever $ QIsExtEnabled_ anything |-> True
  mock $
    whenever $
      qReifyInstances_ ''Show [ConT ''String]
        |-> $(reifyInstancesStatic ''Show [ConT ''String])
  mock $
    whenever $
      qReifyInstances_ ''Eq [ConT ''String]
        |-> $(reifyInstancesStatic ''Eq [ConT ''String])
  mock $
    whenever $
      qReifyInstances_ ''Show [ConT ''Int]
        |-> $(reifyInstancesStatic ''Show [ConT ''Int])
  mock $
    whenever $
      qReifyInstances_ ''Eq [ConT ''Int]
        |-> $(reifyInstancesStatic ''Eq [ConT ''Int])
  mock $
    whenever $
      QReifyInstances_ (eq ''Show) (suchThat isFunctionType)
        |-> []
  mock $
    whenever $
      QReifyInstances_ (eq ''Eq) (suchThat isFunctionType)
        |-> []

isFunctionType :: [Type] -> Bool
isFunctionType = everything (||) (mkQ False isArrow)
  where
    isArrow ArrowT = True
    isArrow _ = False
