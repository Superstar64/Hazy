{-# LANGUAGE RoleAnnotations #-}

module Stage2.Temporary.ExpressionInfix where

import Data.Kind (Type)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity)
import qualified Stage1.Tree.ExpressionInfix as Stage1 (Infix)
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Stage (Resolve)
import Stage2.Temporary.Infix (Infix)
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)

type role Index nominal

type Index :: Environment -> Type
data Index scope

resolve :: Context scope -> Stage1.Infix Position -> Infix (Index scope) (Expression Normal Resolve scope)
fix :: Infix (Index scope) (Expression Normal Resolve scope) -> Expression Normal Resolve scope
fixWith ::
  Maybe Associativity ->
  Int ->
  Infix (Index scope) (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
