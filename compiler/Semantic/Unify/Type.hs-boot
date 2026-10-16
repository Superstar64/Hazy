{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Type where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import qualified Data.Kind as Kind
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import Syntax.Position (Position)
import Prelude hiding (Functor)

type Type s scope = TypeF (Logical s scope) scope

type role Logical nominal nominal

type Logical :: Kind.Type -> Environment -> Kind.Type
data Logical s scopes

instance Shift0.Functor (Logical s)

type role Box nominal nominal

type Box :: Kind.Type -> Environment -> Kind.Type
data Box s scope

fresh :: Type s scope -> ST s (Type s scope)
mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
unshift ::
  forall s scope scopes.
  Context s (scope ':+ scopes) ->
  Position ->
  Type s (scope ':+ scopes) ->
  ST s (Type s scopes)
