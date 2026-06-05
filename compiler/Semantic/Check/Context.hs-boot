{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Context where

import qualified Data.Kind as Kind
import {-# SOURCE #-} Semantic.Check.TypeBinding (TypeBinding)
import qualified Semantic.Index.Table.Type as Type
import Semantic.Scope (Environment)

type role Context nominal nominal

type Context :: Kind.Type -> Environment -> Kind.Type
data Context s scope

typeEnvironment_ :: Context s scope -> Type.Table (TypeBinding s) scope
