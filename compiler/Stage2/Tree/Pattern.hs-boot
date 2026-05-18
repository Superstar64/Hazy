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
import Stage2.Stage (Resolve, Stage)

type role Pattern nominal nominal

type Pattern :: Stage -> Environment -> Type
data Pattern stage scope

instance Show (Pattern stage scope)

instance Shift (Pattern stage)

instance Shift.Functor (Pattern stage)

variable :: Position -> Variable -> Pattern stage scope
neverFails :: Pattern stage scope -> Bool
resolve :: Context scope -> Stage1.Pattern Position -> Pattern Resolve scope
