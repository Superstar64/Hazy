{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Pattern where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Pattern as Stage1 (Pattern)
import Stage1.Variable (Variable)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type role Pattern nominal

type Pattern :: Environment -> Type
data Pattern scope

instance Show (Pattern scope)

instance Shift Pattern

instance Shift.Functor Pattern

variable :: Position -> Variable -> Pattern scope
resolve :: Context scope -> Stage1.Pattern Position -> Pattern scope
