{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Context where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Context nominal

type Context :: Environment -> Type
data Context scope
