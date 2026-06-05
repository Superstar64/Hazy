{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypeDeclaration where

import qualified Core.Tree.Type as Simple
import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Layout (Layout)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Semantic.Tree.TypeDefinition (Constructive, TypeDefinition)
import Syntax.Position (Position)
import Syntax.Variable (Constructor, ConstructorIdentifier)

type role TypeDeclaration nominal nominal nominal nominal

type TypeDeclaration :: Locality -> Layout -> Stage -> Environment -> Type
data TypeDeclaration locality layout stage scope

kind' :: TypeDeclaration locality layout Check scope -> Simple.Type scope

data Groupable scope = Groupable
  { element :: !(TypeDefinition Constructive Resolve scope),
    position' :: !Position,
    name' :: !ConstructorIdentifier,
    constructorNames' :: !(Strict.Vector Constructor)
  }
