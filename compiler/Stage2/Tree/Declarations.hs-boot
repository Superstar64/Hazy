{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declarations where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations)
import Stage2.FreeVariables (FreeTermVariables)
import Stage2.Locality (Locality)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment (..))
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type Declarations :: Locality -> Environment -> Type

type role Declarations phantom nominal

data Declarations locality scope

instance Show (Declarations locality scope)

instance Shift (Declarations locality)

instance Shift.Functor (Declarations locality)

instance FreeTermVariables (Declarations locality)

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations locality (Declaration ':+ scope)
  )
