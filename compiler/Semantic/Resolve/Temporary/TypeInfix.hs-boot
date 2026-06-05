{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Temporary.TypeInfix where

import qualified Data.Kind as Kind (Type)
import Semantic.Resolve.Context (Context)
import Semantic.Resolve.Temporary.Infix (Infix)
import Semantic.Scope (Environment)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Type (Type)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity)
import qualified Syntax.Tree.TypeInfix as Syntax (Infix)

type role Index nominal

type Index :: Environment -> Kind.Type
data Index scope

fixWith ::
  Maybe Associativity ->
  Int ->
  Infix
    (Index scope)
    (Type Position Resolve scope) ->
  Type Position Resolve scope
fix :: Infix (Index scope) (Type Position Resolve scope) -> Type Position Resolve scope
resolve :: Context scope -> Syntax.Infix Position -> Infix (Index scope) (Type Position Resolve scope)
