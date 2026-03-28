{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Expression where

import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Tree.CallHead (CallHead)

type Expression :: Environment -> Type

type role Expression nominal

data Expression scope

instance Show (Expression scope)

instance Shift Expression

instance Shift.Functor Expression

callHead_ :: CallHead scope -> Expression scope
resolve :: Context scope -> Stage1.Expression Position -> Expression scope
