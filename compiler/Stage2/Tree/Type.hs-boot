{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Type where

import qualified Data.Kind as Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Type as Stage1 (Type)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)

type Type :: Kind.Type -> Environment -> Kind.Type

type role Type representational nominal

data Type position scope

instance (Show position) => Show (Type position scope)

resolve :: Context scope -> Stage1.Type Position -> Type Position scope
