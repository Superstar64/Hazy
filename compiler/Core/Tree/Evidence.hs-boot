{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Evidence where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Evidence nominal

type Evidence :: Environment -> Type
data Evidence scope
