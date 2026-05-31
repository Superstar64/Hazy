{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypeDeclaration where

import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage1.Variable (Constructor, ConstructorIdentifier)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Stage2.Tree.TypeDefinition (TypeDefinition)
import qualified Stage4.Tree.Type as Simple

type role TypeDeclaration nominal nominal nominal nominal

type TypeDeclaration :: Locality -> Layout -> Stage -> Environment -> Type
data TypeDeclaration locality layout stage scope

kind' :: TypeDeclaration locality layout Check scope -> Simple.Type scope

data Groupable scope = Groupable
  { element :: !(TypeDefinition Resolve scope),
    position' :: !Position,
    name' :: !ConstructorIdentifier,
    constructorNames' :: !(Strict.Vector Constructor)
  }
