{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Evidence where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Evidence nominal

type Evidence :: Environment -> Type
data Evidence scope
