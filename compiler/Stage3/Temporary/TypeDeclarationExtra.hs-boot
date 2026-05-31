{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Scope (Environment)
import Stage2.Stage (Check)
import {-# SOURCE #-} qualified Stage2.Tree.TypeDeclarationExtra as Solved

type role TypeDeclarationExtra nominal nominal

type TypeDeclarationExtra :: Type -> Environment -> Type
data TypeDeclarationExtra s scope

solve :: TypeDeclarationExtra s scope -> ST s (Solved.TypeDeclarationExtra Group Check scope)
