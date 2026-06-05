{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Statements where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Statements nominal

type Statements :: Environment -> Type
data Statements scope
