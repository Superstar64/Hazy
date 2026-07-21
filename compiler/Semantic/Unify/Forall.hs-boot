{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Forall where

import Control.Monad.ST (ST)
import Core.Tree.Forall (ForallOver)
import Core.Tree.Type (TypeF)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import {-# SOURCE #-} Semantic.Unify.Instanciation
import {-# SOURCE #-} Semantic.Unify.Type
import Syntax.Position (Position)

type Forall s = ForallOver TypeF (Logical s)

instanciate :: Context s scope -> Position -> Forall s scope -> ST s (Type s scope, Instanciation s scope)
