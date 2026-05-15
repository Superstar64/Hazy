{-# LANGUAGE RoleAnnotations #-}

module Stage2.Resolve.Context where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Context nominal

type Context :: Environment -> Type
data Context scope
