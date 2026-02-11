{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Scope (Environment)
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern)
import {-# SOURCE #-} Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Check.KindAnnotation (KindAnnotation)
import {-# SOURCE #-} qualified Stage3.Temporary.TypePattern as Unsolved
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope

infix 4 :^

type role TypeDeclaration nominal

type TypeDeclaration :: Environment -> Type
data TypeDeclaration scope

kind'_ :: TypeDeclaration scope -> Simple.Type scope
checkHead ::
  (Traversable t) =>
  Context s scope ->
  Position ->
  KindAnnotation scope ->
  (t (Stage2.TypePattern Position), Unify.Type s scope) ->
  ST s (t (Unsolved.TypePattern s scope), Unify.Type s scope)
