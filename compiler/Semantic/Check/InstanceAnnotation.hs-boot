{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.InstanceAnnotation where

import qualified Core.Tree.Constraints as Simple
import Data.Kind (Type)
import Semantic.Scope (Environment)

type role InstanceAnnotation nominal

type InstanceAnnotation :: Environment -> Type
data InstanceAnnotation scope

prerequisites'_ :: InstanceAnnotation scope -> Simple.Constraints scope
