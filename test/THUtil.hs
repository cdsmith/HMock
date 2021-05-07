{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module THUtil where

import Control.DeepSeq (NFData (..))

import Language.Haskell.TH (Q, Name, Exp, Type)
import Language.Haskell.TH.Syntax (Lift(..), reify, reifyInstances)
import qualified Language.Haskell.TH.Syntax

reifyStatic :: Name -> Q Exp
reifyStatic n = reify n >>= lift

reifyInstancesStatic :: Name -> [Type] -> Q Exp
reifyInstancesStatic n ts = reifyInstances n ts >>= lift

deriving instance Lift Language.Haskell.TH.Syntax.AnnTarget

deriving instance Lift Language.Haskell.TH.Syntax.Bang

deriving instance Lift Language.Haskell.TH.Syntax.Body

deriving instance Lift Language.Haskell.TH.Syntax.Callconv

deriving instance Lift Language.Haskell.TH.Syntax.Clause

deriving instance Lift Language.Haskell.TH.Syntax.Con

deriving instance Lift Language.Haskell.TH.Syntax.Dec

deriving instance Lift Language.Haskell.TH.Syntax.DerivClause

deriving instance Lift Language.Haskell.TH.Syntax.DerivStrategy

deriving instance Lift Language.Haskell.TH.Syntax.Exp

deriving instance Lift Language.Haskell.TH.Syntax.FamilyResultSig

deriving instance Lift Language.Haskell.TH.Syntax.Fixity

deriving instance Lift Language.Haskell.TH.Syntax.FixityDirection

deriving instance Lift Language.Haskell.TH.Syntax.Foreign

deriving instance Lift Language.Haskell.TH.Syntax.FunDep

deriving instance Lift Language.Haskell.TH.Syntax.Guard

deriving instance Lift Language.Haskell.TH.Syntax.Info

deriving instance Lift Language.Haskell.TH.Syntax.InjectivityAnn

deriving instance Lift Language.Haskell.TH.Syntax.Inline

deriving instance Lift Language.Haskell.TH.Syntax.Lit

deriving instance Lift Language.Haskell.TH.Syntax.Match

deriving instance Lift Language.Haskell.TH.Syntax.ModName

deriving instance Lift Language.Haskell.TH.Syntax.Name

deriving instance Lift Language.Haskell.TH.Syntax.NameFlavour

deriving instance Lift Language.Haskell.TH.Syntax.NameSpace

deriving instance Lift Language.Haskell.TH.Syntax.OccName

deriving instance Lift Language.Haskell.TH.Syntax.Overlap

deriving instance Lift Language.Haskell.TH.Syntax.Pat

deriving instance Lift Language.Haskell.TH.Syntax.PatSynArgs

deriving instance Lift Language.Haskell.TH.Syntax.PatSynDir

deriving instance Lift Language.Haskell.TH.Syntax.Phases

deriving instance Lift Language.Haskell.TH.Syntax.PkgName

deriving instance Lift Language.Haskell.TH.Syntax.Pragma

deriving instance Lift Language.Haskell.TH.Syntax.Range

deriving instance Lift Language.Haskell.TH.Syntax.Role

deriving instance Lift Language.Haskell.TH.Syntax.RuleBndr

deriving instance Lift Language.Haskell.TH.Syntax.RuleMatch

deriving instance Lift Language.Haskell.TH.Syntax.Safety

deriving instance Lift Language.Haskell.TH.Syntax.SourceStrictness

deriving instance Lift Language.Haskell.TH.Syntax.SourceUnpackedness

deriving instance Lift Language.Haskell.TH.Syntax.Stmt

deriving instance Lift Language.Haskell.TH.Syntax.TyLit

deriving instance Lift Language.Haskell.TH.Syntax.Type

deriving instance Lift Language.Haskell.TH.Syntax.TypeFamilyHead

deriving instance Lift Language.Haskell.TH.Syntax.TySynEqn

deriving instance Lift Language.Haskell.TH.Syntax.TyVarBndr

instance Lift Language.Haskell.TH.Syntax.Bytes where
  lift = undefined
  liftTyped = undefined

instance NFData Language.Haskell.TH.Syntax.AnnTarget

instance NFData Language.Haskell.TH.Syntax.Bang

instance NFData Language.Haskell.TH.Syntax.Body

instance NFData Language.Haskell.TH.Syntax.Callconv

instance NFData Language.Haskell.TH.Syntax.Clause

instance NFData Language.Haskell.TH.Syntax.Con

instance NFData Language.Haskell.TH.Syntax.Dec

instance NFData Language.Haskell.TH.Syntax.DerivClause

instance NFData Language.Haskell.TH.Syntax.DerivStrategy

instance NFData Language.Haskell.TH.Syntax.Exp

instance NFData Language.Haskell.TH.Syntax.FamilyResultSig

instance NFData Language.Haskell.TH.Syntax.Fixity

instance NFData Language.Haskell.TH.Syntax.FixityDirection

instance NFData Language.Haskell.TH.Syntax.Foreign

instance NFData Language.Haskell.TH.Syntax.FunDep

instance NFData Language.Haskell.TH.Syntax.Guard

instance NFData Language.Haskell.TH.Syntax.Info

instance NFData Language.Haskell.TH.Syntax.InjectivityAnn

instance NFData Language.Haskell.TH.Syntax.Inline

instance NFData Language.Haskell.TH.Syntax.Lit

instance NFData Language.Haskell.TH.Syntax.Match

instance NFData Language.Haskell.TH.Syntax.ModName

instance NFData Language.Haskell.TH.Syntax.Name

instance NFData Language.Haskell.TH.Syntax.NameFlavour

instance NFData Language.Haskell.TH.Syntax.NameSpace

instance NFData Language.Haskell.TH.Syntax.OccName

instance NFData Language.Haskell.TH.Syntax.Overlap

instance NFData Language.Haskell.TH.Syntax.Pat

instance NFData Language.Haskell.TH.Syntax.PatSynArgs

instance NFData Language.Haskell.TH.Syntax.PatSynDir

instance NFData Language.Haskell.TH.Syntax.Phases

instance NFData Language.Haskell.TH.Syntax.PkgName

instance NFData Language.Haskell.TH.Syntax.Pragma

instance NFData Language.Haskell.TH.Syntax.Range

instance NFData Language.Haskell.TH.Syntax.Role

instance NFData Language.Haskell.TH.Syntax.RuleBndr

instance NFData Language.Haskell.TH.Syntax.RuleMatch

instance NFData Language.Haskell.TH.Syntax.Safety

instance NFData Language.Haskell.TH.Syntax.SourceStrictness

instance NFData Language.Haskell.TH.Syntax.SourceUnpackedness

instance NFData Language.Haskell.TH.Syntax.Stmt

instance NFData Language.Haskell.TH.Syntax.TyLit

instance NFData Language.Haskell.TH.Syntax.Type

instance NFData Language.Haskell.TH.Syntax.TypeFamilyHead

instance NFData Language.Haskell.TH.Syntax.TySynEqn

instance NFData Language.Haskell.TH.Syntax.TyVarBndr

instance NFData Language.Haskell.TH.Syntax.Bytes where
  rnf = undefined
