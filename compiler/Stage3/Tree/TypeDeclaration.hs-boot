{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TypeDeclaration where

import Data.Kind (Type)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Scope (Environment)
import qualified Stage4.Tree.Type as Simple

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope

infix 4 :^

type role TypeDeclaration nominal

type TypeDeclaration :: Environment -> Type
data TypeDeclaration scope

kind'_ :: TypeDeclaration scope -> Simple.Type scope
