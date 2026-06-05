{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Type where

import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.Type as Semantic

data Type scope
  = Variable !(Local.Index scope)
  | Constructor !(Type2.Index scope)
  | Call !(Type scope) !(Type scope)
  | Function !(Type scope) !(Type scope)
  | Type !(Type scope)
  | Constraint
  | Small
  | Large
  | Universe
  | Levity

instance Eq (Type scope)

instance Show (Type scope)

instance Shift Type

instance Shift.Functor Type

instance Scope.Show Type

smallType :: Type scope
simplify :: Semantic.Type position Check scope -> Type scope
