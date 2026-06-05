{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.InstanceAnnotation where

import qualified Core.Tree.Constraint as Simple
import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Scope (Environment)

type role InstanceAnnotation nominal

type InstanceAnnotation :: Environment -> Type
data InstanceAnnotation scope

prerequisites'_ :: InstanceAnnotation scope -> Strict.Vector (Simple.Constraint scope)
