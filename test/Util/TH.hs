{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Util.TH (deriveRecursive, reifyStatic, reifyInstancesStatic) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util.DeriveRecursive (deriveRecursive)

#if MIN_VERSION_template_haskell(2, 16, 0)
-- Pre-define low-level instance to prevent deriveRecursive from trying.
instance Lift Bytes where lift = undefined; liftTyped = undefined
#endif

deriveRecursive Nothing ''Lift ''Info

reifyStatic :: Name -> Q Exp
reifyStatic n = reify n >>= lift

deriveRecursive Nothing ''Lift ''InstanceDec

reifyInstancesStatic :: Name -> [Type] -> Q Exp
reifyInstancesStatic n ts = reifyInstances n ts >>= lift
