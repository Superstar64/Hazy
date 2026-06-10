{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Temporary.ExpressionInfix where

import Data.Kind (Type)
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import Semantic.Resolve.Temporary.Infix (Infix)
import Semantic.Scope (Environment)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity)
import qualified Syntax.Tree.ExpressionInfix as Syntax (Infix)

type role Index nominal

type Index :: Environment -> Type
data Index scope

data Negate

resolve :: Context scope -> Syntax.Infix Position -> Infix Negate (Index scope) (Expression Normal Resolve scope)
fix :: Infix Negate (Index scope) (Expression Normal Resolve scope) -> Expression Normal Resolve scope
fixWith ::
  Maybe Associativity ->
  Int ->
  Infix Negate (Index scope) (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
