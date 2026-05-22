{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Expression where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import Stage2.Connect (Connect)
import Stage2.FreeVariables (FreeTermVariables)
import Stage2.Layout (Layout, Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve, Stage)
import Stage2.Tree.CallHead (CallHead)

type Expression :: Layout -> Stage -> Environment -> Type

type role Expression nominal nominal nominal

data Expression layout stage scope

instance Show (Expression layout stage scope)

instance Shift (Expression layout stage)

instance Shift.Functor (Expression layout stage)

instance FreeTermVariables (Expression layout)

instance Connect Expression

callHead_ :: CallHead Resolve scope -> Expression Normal Resolve scope
resolve :: Context scope -> Stage1.Expression Position -> Expression Normal Resolve scope
