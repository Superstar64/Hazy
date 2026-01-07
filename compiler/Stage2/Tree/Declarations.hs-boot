{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declarations where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment (..))
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type Declarations :: Environment -> Type

type role Declarations nominal

data Declarations scope

instance Show (Declarations scope)

instance Shift Declarations

instance Shift.Functor Declarations

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations (Declaration ':+ scope)
  )
