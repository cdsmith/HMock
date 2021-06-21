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

module QuasiMockBase where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.HMock.TH (deriveMockableBase)
import Util.DeriveRecursive (deriveRecursive)

#if MIN_VERSION_template_haskell(2, 16, 0)
-- Pre-define low-level instance to prevent deriveRecursive from trying.
instance Lift Bytes where lift = undefined; liftTyped = undefined
#endif

deriveRecursive Nothing ''Lift ''Info

deriveMockableBase ''Quasi

reifyStatic :: Name -> Q Exp
reifyStatic n = reify n >>= lift

onReify :: Q Exp -> Name -> Q Exp
onReify handler n = do
  result <- reify n
  [| $handler (QReify $(lift n) |-> $(lift result))|]

deriveRecursive Nothing ''Lift ''InstanceDec

reifyInstancesStatic :: Name -> [Type] -> Q Exp
reifyInstancesStatic n ts = reifyInstances n ts >>= lift

onReifyInstances :: Q Exp -> Name -> [Type] -> Q Exp
onReifyInstances handler n ts = do
  result <- reifyInstances n ts
  [|$handler (QReifyInstances $(lift n) $(lift ts) |-> $(lift result))|]