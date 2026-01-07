{-# LANGUAGE RoleAnnotations #-}

module Stage3.Simple.Constraint where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Constraint as Solved

type role Constraint nominal

type Constraint :: Environment -> Type
data Constraint scope

instance Show (Constraint scope)

instance Shift Constraint

instance Shift.Functor Constraint

simplify :: Solved.Constraint scope -> Constraint scope
