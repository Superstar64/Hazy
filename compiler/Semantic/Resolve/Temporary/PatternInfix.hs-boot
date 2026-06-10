{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Temporary.PatternInfix where

import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Resolve.Context (Context)
import Semantic.Resolve.Temporary.Infix (Infix)
import Semantic.Scope (Environment)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Pattern (Pattern)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity)
import qualified Syntax.Tree.PatternInfix as Syntax (Infix)

type role Index nominal

type Index :: Environment -> Type
data Index scope

resolve :: Context scope -> Syntax.Infix Position -> Infix Void (Index scope) (Pattern Resolve scope)
fixWith :: Maybe Associativity -> Int -> Infix Void (Index scope) (Pattern Resolve scope) -> Pattern Resolve scope
fix :: Infix Void (Index scope) (Pattern Resolve scope) -> Pattern Resolve scope
