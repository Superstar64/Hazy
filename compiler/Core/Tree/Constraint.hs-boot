{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Constraint where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraint as Solved

type role Constraint nominal

type Constraint :: Environment -> Type
data Constraint scope

instance Show (Constraint scope)

instance Shift Constraint

instance Shift.Functor Constraint

simplify :: Solved.Constraint position Check scope -> Constraint scope
