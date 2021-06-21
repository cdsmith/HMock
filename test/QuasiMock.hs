{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiMock (module QuasiMock, module QuasiMockBase) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.Generics (Typeable, everything, mkQ)
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import Test.HMock
import QuasiMockBase

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

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

    $(expectReify ''String)
    $(expectReify ''Char)
    $(expectReify ''Int)
    $(expectReify ''Bool)
    $(expectReify ''Enum)
    $(expectReify ''Monad)

    $(expectReifyInstances ''Show [ConT ''String])
    $(expectReifyInstances ''Eq [ConT ''String])
    $(expectReifyInstances ''Show [ConT ''Char])
    $(expectReifyInstances ''Eq [ConT ''Char])
    $(expectReifyInstances ''Show [ConT ''Int])
    $(expectReifyInstances ''Eq [ConT ''Int])
    $(expectReifyInstances ''Show [ConT ''Bool])
    $(expectReifyInstances ''Eq [ConT ''Bool])
    $(expectReifyInstances ''Default [TupleT 0])
    $(expectReifyInstances ''Default [ConT ''String])
    $(expectReifyInstances ''Default [ConT ''Int])
    $(expectReifyInstances ''Default [AppT (ConT ''Maybe) (ConT ''Bool)])

    expectAny $ QReifyInstances_ (eq ''Show) (is functionType) |-> []
    expectAny $ QReifyInstances_ (eq ''Eq) (is functionType) |-> []

    expectAny $
      QReifyInstances_
        (eq ''Show)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Show [AppT ListT (VarT (mkName "a"))])
    expectAny $
      QReifyInstances_
        (eq ''Eq)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Eq [AppT ListT (VarT (mkName "a"))])