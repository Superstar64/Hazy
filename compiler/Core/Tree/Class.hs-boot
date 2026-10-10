{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Class where

import {-# SOURCE #-} Core.Tree.Type (Type)
import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type role Class nominal

type Class :: Semantic.Scope.Environment -> Kind.Type
data Class scope

instance Shift0.Functor Class

instance Shift.Functor Class

kind :: Class scope -> Type scope
