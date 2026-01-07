{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TermDeclaration where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage3.Simple.Scheme as Simple

type role TermDeclaration nominal

type TermDeclaration :: Environment -> Type
data TermDeclaration scope

simple :: TermDeclaration scope -> Simple.Scheme scope
