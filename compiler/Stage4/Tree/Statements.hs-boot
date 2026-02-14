{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Statements where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Statements nominal

type Statements :: Environment -> Type
data Statements scope
