{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypeDeclaration where

import qualified Core.Tree.Type as Simple
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Layout (Layout)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Semantic.Tree.TypeDefinition (TypeDefinition)
import Syntax.Position (Position)
import Syntax.Variable (Constructor, ConstructorIdentifier)

type role TypeDeclaration representational nominal nominal nominal nominal

type TypeDeclaration :: (Type -> Type) -> Locality -> Layout -> Stage -> Environment -> Type
data TypeDeclaration loeb locality layout stage scope

kind' :: TypeDeclaration Identity locality layout Check scope -> Simple.Type scope

data Groupable scope = Groupable
  { element :: !(TypeDefinition Resolve scope),
    position' :: !Position,
    name' :: !ConstructorIdentifier,
    constructorNames' :: !(Strict.Vector Constructor)
  }
