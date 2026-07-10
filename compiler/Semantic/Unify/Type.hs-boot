{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Type where

import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Simple
import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import {-# SOURCE #-} Semantic.Unify.Class (Solve, Zonk)
import Syntax.Position (Position)
import Prelude hiding (Functor)

newtype Type s scopes = Typex {runTypex :: TypeF (Logical s) scopes}

instance Shift (Type s)

instance Zonk Type

type role Logical nominal nominal

type Logical :: Kind.Type -> Environment -> Kind.Type
data Logical s scopes

instance Shift (Logical s)

instance Shift.Functor (Logical s)

type role Box nominal nominal

type Box :: Kind.Type -> Environment -> Kind.Type
data Box s scope

solve :: Position -> TypeF (Logical s) scope -> Solve s (Simple.Type scope)
