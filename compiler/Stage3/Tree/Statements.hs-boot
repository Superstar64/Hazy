{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Statements (Statements, Syntax, Guard, Do) where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import {-# SOURCE #-} Stage2.Tree.Statements (Do, Guard, Syntax)

type role Statements nominal nominal

type Statements :: Syntax -> Environment -> Type
data Statements syntax scope

instance Show (Statements syntax scope)
