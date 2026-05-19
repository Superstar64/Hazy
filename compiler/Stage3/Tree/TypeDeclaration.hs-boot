{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TypeDeclaration where

import Data.Kind (Type)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import qualified Stage4.Tree.Type as Simple

data LazyTypeDeclaration locality layout scope
  = !ConstructorIdentifier :^ TypeDeclaration locality layout scope

infix 4 :^

type role TypeDeclaration nominal nominal nominal

type TypeDeclaration :: Locality -> Layout -> Environment -> Type
data TypeDeclaration locality layout scope

kind_ :: TypeDeclaration locality layout scope -> Simple.Type scope
