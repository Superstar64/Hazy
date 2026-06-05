{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.TypeDeclarationExtra where

import Data.Kind (Type)
import Semantic.Layout (Group)
import Semantic.Scope (Environment)
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.TypeDeclarationExtra as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify

type role TypeDeclarationExtra nominal nominal

type TypeDeclarationExtra :: Type -> Environment -> Type
data TypeDeclarationExtra s scope

solve :: TypeDeclarationExtra s scope -> Unify.Solve s (Solved.TypeDeclarationExtra Group Check scope)
