{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.ClassExtra where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role ClassExtra nominal

type ClassExtra :: Environment -> Type
data ClassExtra scope

instance Show (ClassExtra scope)
