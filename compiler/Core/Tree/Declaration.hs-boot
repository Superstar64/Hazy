{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Declaration where

import Core.Tree.TypeLambda (TypeLambdaOver)
import Data.Kind (Type)
import qualified Semantic.Check.Go.Scheme as Semantic
import Semantic.Layout (Normal)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Expression as Semantic

type role Declaration nominal

type Declaration :: Environment -> Type
data Declaration scope

instance Shift0.Functor Declaration

annotation ::
  TypeLambdaOver (Semantic.Expression Normal Check) scope ->
  Semantic.Scheme position Check scope ->
  Declaration scope
