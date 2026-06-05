{-# LANGUAGE RoleAnnotations #-}

module Semantic.Index.Term where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Index nominal

type Index :: Environment -> Type
data Index scopes
