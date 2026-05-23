{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypeDeclaration where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Stage)
import qualified Stage4.Tree.Type as Simple

type role TypeDeclaration nominal nominal nominal nominal

type TypeDeclaration :: Locality -> Layout -> Stage -> Environment -> Type
data TypeDeclaration locality layout stage scope

kind' :: TypeDeclaration locality layout Check scope -> Simple.Type scope
