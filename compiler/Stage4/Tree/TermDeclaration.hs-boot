{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.TermDeclaration where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage3.Tree.Expression as Stage3
import qualified Stage3.Tree.Scheme as Stage3
import Stage4.Tree.SchemeOver (SchemeOver)

type role LazyTermDeclaration nominal

type LazyTermDeclaration :: Environment -> Type
data LazyTermDeclaration scope

instance Shift LazyTermDeclaration

annotation :: SchemeOver Stage3.Expression scope -> Stage3.Scheme scope -> LazyTermDeclaration scope
