{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Type where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import {-# SOURCE #-} Stage3.Unify.Class (Zonk)
import qualified Stage4.Tree.Type as Simple

type role Type nominal nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

instance Shift (Type s)

instance Zonk Type

type role Box nominal nominal

type Box :: Kind.Type -> Environment -> Kind.Type
data Box s scope

fresh :: Type s scope -> ST s (Type s scope)
-- todo, have mechinism to ensure solve is the last ST action
solve :: Position -> Type s scope -> ST s (Simple.Type scope)
