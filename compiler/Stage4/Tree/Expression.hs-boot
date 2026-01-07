{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Expression where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Expression nominal

type Expression :: Environment -> Type
data Expression scope

instance Show (Expression scope)
