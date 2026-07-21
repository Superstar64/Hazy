{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Type where

import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Simple
import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Unify.Solve (Solve)
import Syntax.Position (Position)
import Prelude hiding (Functor)

type Type s = TypeF (Logical s)

type role Logical nominal nominal

type Logical :: Kind.Type -> Environment -> Kind.Type
data Logical s scopes

instance Shift0.Functor (Logical s)

instance Shift.Functor (Logical s)

type role Box nominal nominal

type Box :: Kind.Type -> Environment -> Kind.Type
data Box s scope

solve :: Position -> TypeF (Logical s) scope -> Solve s (Simple.Type scope)
