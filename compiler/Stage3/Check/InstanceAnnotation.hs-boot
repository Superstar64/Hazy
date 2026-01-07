{-# LANGUAGE RoleAnnotations #-}

module Stage3.Check.InstanceAnnotation where

import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment)
import qualified Stage3.Simple.Constraint as Simple

type role InstanceAnnotation nominal

type InstanceAnnotation :: Environment -> Type
data InstanceAnnotation scope

prerequisites'_ :: InstanceAnnotation scope -> Strict.Vector (Simple.Constraint scope)
