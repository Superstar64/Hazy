{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Type where

import qualified Data.Kind as Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Type as Stage1 (Type)
import {-# SOURCE #-} Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Stage (Resolve, Stage)

type Type :: Kind.Type -> Stage -> Environment -> Kind.Type

type role Type representational nominal nominal

data Type position stage scope

instance (Show position) => Show (Type position stage scope)

resolve :: Context scope -> Stage1.Type Position -> Type Position Resolve scope
