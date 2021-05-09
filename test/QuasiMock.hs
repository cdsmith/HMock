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

module QuasiMock where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Generics (Typeable, everything, mkQ)
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import qualified Language.Haskell.TH.Syntax
import THUtil (deriveRecursive)
import Test.HMock
import Test.HMock.TH

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

-- Pre-define low-level instance to prevent deriveRecursive from trying.
instance Lift Bytes where lift = undefined; liftTyped = undefined

deriveRecursive Nothing ''Lift ''Info
deriveRecursive Nothing ''Lift ''InstanceDec

reifyStatic :: Name -> Q Exp
reifyStatic n = reify n >>= lift

reifyInstancesStatic :: Name -> [Type] -> Q Exp
reifyInstancesStatic n ts = reifyInstances n ts >>= lift

isFunctionType :: [Type] -> Bool
isFunctionType = everything (||) (mkQ False isArrow)
  where
    isArrow ArrowT = True
    isArrow _ = False
