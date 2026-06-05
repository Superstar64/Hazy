{-# LANGUAGE RoleAnnotations #-}

module Stage2.Unify.Type where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import {-# SOURCE #-} Stage2.Check.Context (Context)
import qualified Stage2.Check.Mask as Mask
import {-# SOURCE #-} Stage2.Unify.Class (Functor, Solve, Zonk)
import qualified Stage4.Tree.Type as Simple
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
