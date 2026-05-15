{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Type where

import Stage1.Position (Position)
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import {-# SOURCE #-} qualified Stage2.Tree.Type as Stage3

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

smallType :: Type scope
simplify :: Stage3.Type Position Check scope -> Type scope
