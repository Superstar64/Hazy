{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Type where

import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.Type as Stage3

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

instance Eq (Type scope)

instance Show (Type scope)

instance Shift Type

instance Shift.Functor Type

smallType :: Type scope
substitute ::
  (Local.Index scope1 -> Type scope2) ->
  (Type2.Index scope1 -> Type scope2) ->
  Type scope1 ->
  Type scope2
simplify :: Stage3.Type scope -> Type scope
