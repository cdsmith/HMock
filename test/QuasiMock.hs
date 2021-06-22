{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiMock (module QuasiMock, module QuasiMockBase) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.Generics (Typeable, everything, mkQ)
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import QuasiMockBase
import Test.HMock

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
    byDefault $ QIsExtEnabled_ anything |-> True

    $(onReify [|byDefault|] ''String)
    $(onReify [|byDefault|] ''Char)
    $(onReify [|byDefault|] ''Int)
    $(onReify [|byDefault|] ''Bool)
    $(onReify [|byDefault|] ''Enum)
    $(onReify [|byDefault|] ''Monad)

    $(onReifyInstances [|byDefault|] ''Show [ConT ''String])
    $(onReifyInstances [|byDefault|] ''Eq [ConT ''String])
    $(onReifyInstances [|byDefault|] ''Show [ConT ''Char])
    $(onReifyInstances [|byDefault|] ''Eq [ConT ''Char])
    $(onReifyInstances [|byDefault|] ''Show [ConT ''Int])
    $(onReifyInstances [|byDefault|] ''Eq [ConT ''Int])
    $(onReifyInstances [|byDefault|] ''Show [ConT ''Bool])
    $(onReifyInstances [|byDefault|] ''Eq [ConT ''Bool])
    $(onReifyInstances [|byDefault|] ''Default [TupleT 0])
    $(onReifyInstances [|byDefault|] ''Default [ConT ''String])
    $(onReifyInstances [|byDefault|] ''Default [ConT ''Int])
    $( onReifyInstances
         [|byDefault|]
         ''Default
         [AppT (ConT ''Maybe) (ConT ''Bool)]
     )

    byDefault $ QReifyInstances_ (eq ''Show) (is functionType) |-> []
    byDefault $ QReifyInstances_ (eq ''Eq) (is functionType) |-> []

    byDefault $
      QReifyInstances_
        (eq ''Show)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Show [AppT ListT (VarT (mkName "a"))])
    byDefault $
      QReifyInstances_
        (eq ''Eq)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Eq [AppT ListT (VarT (mkName "a"))])