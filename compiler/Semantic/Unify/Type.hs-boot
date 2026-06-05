{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Type where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Simple
import qualified Data.Kind as Kind
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import {-# SOURCE #-} Semantic.Unify.Class (Functor, Solve, Zonk)
import Syntax.Position (Position)
import Prelude hiding (Functor)

type role Type nominal nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

instance Shift (Type s)

instance Zonk Type

instance Functor (Type s)

type role Box nominal nominal

type Box :: Kind.Type -> Environment -> Kind.Type
data Box s scope

fresh :: Type s scope -> ST s (Type s scope)
mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
solve :: Position -> Type s scope -> Solve s (Simple.Type scope)
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
