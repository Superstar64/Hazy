{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Declarations where

import Data.Kind (Type)
import Semantic.FreeVariables (FreeTermVariables)
import Semantic.Layout (Group, Layout, Normal)
import Semantic.Locality (Local, Locality)
import Semantic.Scope (Declaration, Environment (..))
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve, Stage)

type Declarations :: Locality -> Layout -> Stage -> Environment -> Type

type role Declarations nominal nominal nominal nominal

data Declarations locality layout stage scope

instance Show (Declarations locality layout stage scope)

instance Shift (Declarations locality layout stage)

instance Shift.Functor (Declarations locality layout stage)

instance FreeTermVariables (Declarations locality layout)

connect ::
  forall scope.
  Declarations Local Normal Resolve (Declaration ':+ scope) ->
  Declarations Local Group Resolve (Declaration ':+ scope)
seperate ::
  forall scope.
  Declarations Local Group Check (Declaration ':+ scope) ->
  Declarations Local Normal Check (Declaration ':+ scope)
