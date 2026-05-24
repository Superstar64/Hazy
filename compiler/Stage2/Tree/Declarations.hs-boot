{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declarations where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations)
import Stage2.FreeVariables (FreeTermVariables)
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Local, Locality)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment (..))
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve, Stage)

type Declarations :: Locality -> Layout -> Stage -> Environment -> Type

type role Declarations nominal nominal nominal nominal

data Declarations locality layout stage scope

instance Show (Declarations locality layout stage scope)

instance Shift (Declarations locality layout stage)

instance Shift.Functor (Declarations locality layout stage)

instance FreeTermVariables (Declarations locality layout)

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations locality Normal Resolve (Declaration ':+ scope)
  )
connect ::
  forall scope.
  Declarations Local Normal Resolve (Declaration ':+ scope) ->
  Declarations Local Group Resolve (Declaration ':+ scope)
seperate ::
  forall scope.
  Declarations Local Group Check (Declaration ':+ scope) ->
  Declarations Local Normal Check (Declaration ':+ scope)
