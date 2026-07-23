{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Type where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Simple
import qualified Data.Kind as Kind
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
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

fresh :: Type s scope -> ST s (Type s scope)
mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
solve :: Position -> TypeF (Logical s) scope -> Solve s (Simple.Type scope)
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
