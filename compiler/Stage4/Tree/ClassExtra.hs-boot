{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.ClassExtra where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role ClassExtra nominal

type ClassExtra :: Environment -> Type
data ClassExtra scope

instance Show (ClassExtra scope)
