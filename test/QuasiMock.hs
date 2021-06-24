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
    allowUnexpected $ QIsExtEnabled_ anything |-> True

    $(onReify [|allowUnexpected|] ''String)
    $(onReify [|allowUnexpected|] ''Char)
    $(onReify [|allowUnexpected|] ''Int)
    $(onReify [|allowUnexpected|] ''Bool)
    $(onReify [|allowUnexpected|] ''Enum)
    $(onReify [|allowUnexpected|] ''Monad)

    $(onReifyInstances [|allowUnexpected|] ''Show [ConT ''String])
    $(onReifyInstances [|allowUnexpected|] ''Eq [ConT ''String])
    $(onReifyInstances [|allowUnexpected|] ''Show [ConT ''Char])
    $(onReifyInstances [|allowUnexpected|] ''Eq [ConT ''Char])
    $(onReifyInstances [|allowUnexpected|] ''Show [ConT ''Int])
    $(onReifyInstances [|allowUnexpected|] ''Eq [ConT ''Int])
    $(onReifyInstances [|allowUnexpected|] ''Show [ConT ''Bool])
    $(onReifyInstances [|allowUnexpected|] ''Eq [ConT ''Bool])
    $(onReifyInstances [|allowUnexpected|] ''Default [TupleT 0])
    $(onReifyInstances [|allowUnexpected|] ''Default [ConT ''String])
    $(onReifyInstances [|allowUnexpected|] ''Default [ConT ''Int])
    $( onReifyInstances
         [|allowUnexpected|]
         ''Default
         [AppT (ConT ''Maybe) (ConT ''Bool)]
     )

    allowUnexpected $ QReifyInstances_ (eq ''Show) (is functionType) |-> []
    allowUnexpected $ QReifyInstances_ (eq ''Eq) (is functionType) |-> []

    allowUnexpected $
      QReifyInstances_
        (eq ''Show)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Show [AppT ListT (VarT (mkName "a"))])
    allowUnexpected $
      QReifyInstances_
        (eq ''Eq)
        (elemsAre [$(qMatch [p|AppT ListT (VarT _)|])])
        |-> $(reifyInstancesStatic ''Eq [AppT ListT (VarT (mkName "a"))])