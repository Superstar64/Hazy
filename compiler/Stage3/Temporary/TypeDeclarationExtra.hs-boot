{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclarationExtra as Solved

type role TypeDeclarationExtra nominal nominal

type TypeDeclarationExtra :: Type -> Environment -> Type
data TypeDeclarationExtra s scope

solve :: TypeDeclarationExtra s scope -> ST s (Solved.TypeDeclarationExtra scope)
