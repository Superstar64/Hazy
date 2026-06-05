{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Pattern where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Stage)
import Syntax.Position (Position)
import Syntax.Variable (Variable)

type role Pattern nominal nominal

type Pattern :: Stage -> Environment -> Type
data Pattern stage scope

instance Show (Pattern stage scope)

instance Shift (Pattern stage)

instance Shift.Functor (Pattern stage)

variable :: Position -> Variable -> Pattern stage scope
neverFails :: Pattern stage scope -> Bool
