{-# LANGUAGE RoleAnnotations #-}

module Semantic.Index.Constructor where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Scope (Environment (..), Local)
import {-# SOURCE #-} qualified Semantic.Shift as Shift
import {-# SOURCE #-} qualified Semantic.Shift0 as Shift0

type role Index nominal

type Index :: Environment -> Type
data Index scope

instance Show (Index scope)

instance Eq (Index scope)

instance Ord (Index scope)

instance Shift0.Functor Index

instance Shift.Functor Index

unlocal :: Index (Local ':+ scope) -> Index scope
partialUnshift :: (Applicative f) => f Void -> Index (scope1 ':+ scope2) -> f (Index scope2)
