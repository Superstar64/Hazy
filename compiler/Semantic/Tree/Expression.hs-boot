{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Expression where

import Data.Kind (Type)
import Semantic.Connect (Connect)
import Semantic.FreeVariables (FreeTermVariables)
import Semantic.Layout (Layout, Normal)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Resolve, Stage)
import Semantic.Tree.CallHead (CallHead)

type Expression :: Layout -> Stage -> Environment -> Type

type role Expression nominal nominal nominal

data Expression layout stage scope

instance Show (Expression layout stage scope)

instance Shift (Expression layout stage)

instance Shift.Functor (Expression layout stage)

instance FreeTermVariables (Expression layout)

instance Connect Expression

callHead_ :: CallHead Resolve scope -> Expression Normal Resolve scope
