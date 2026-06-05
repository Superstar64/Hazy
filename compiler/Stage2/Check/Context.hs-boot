{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Context where

import qualified Data.Kind as Kind
import qualified Stage2.Index.Table.Type as Type
import Stage2.Scope (Environment)
import {-# SOURCE #-} Stage2.Check.TypeBinding (TypeBinding)

type role Context nominal nominal

type Context :: Kind.Type -> Environment -> Kind.Type
data Context s scope

typeEnvironment_ :: Context s scope -> Type.Table (TypeBinding s) scope
