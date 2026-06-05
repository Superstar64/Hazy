{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Temporary.TypeDeclarationExtra where

import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Scope (Environment)
import Stage2.Stage (Check)
import {-# SOURCE #-} qualified Stage2.Tree.TypeDeclarationExtra as Solved
import {-# SOURCE #-} qualified Stage2.Unify as Unify

type role TypeDeclarationExtra nominal nominal

type TypeDeclarationExtra :: Type -> Environment -> Type
data TypeDeclarationExtra s scope

solve :: TypeDeclarationExtra s scope -> Unify.Solve s (Solved.TypeDeclarationExtra Group Check scope)
