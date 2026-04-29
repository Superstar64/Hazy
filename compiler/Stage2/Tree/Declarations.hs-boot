{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declarations where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations)
import Stage2.FreeVariables (FreeTermVariables)
import Stage2.Layout (Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment (..))
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type Declarations :: Locality -> Layout -> Environment -> Type

type role Declarations nominal nominal nominal

data Declarations locality layout scope

instance Show (Declarations locality layout scope)

instance Shift (Declarations locality layout)

instance Shift.Functor (Declarations locality layout)

instance FreeTermVariables (Declarations locality layout)

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations locality Normal (Declaration ':+ scope)
  )
