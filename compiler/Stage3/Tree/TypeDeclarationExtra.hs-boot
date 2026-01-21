{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TypeDeclarationExtra where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role TypeDeclarationExtra nominal

type TypeDeclarationExtra :: Environment -> Type
data TypeDeclarationExtra scope
