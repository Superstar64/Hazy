{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Temporary.Complete.Declarations where

import Data.Kind (Type)
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Term0 as Term0
import qualified Semantic.Index.Type0 as Type0
import Semantic.Layout (Normal)
import Semantic.Resolve.Bindings (Bindings)
import Semantic.Resolve.Context (Context)
import Semantic.Scope (Environment)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} qualified Semantic.Tree.Declarations as Real
import Syntax.Extensions (Extensions)
import Syntax.Position (Position)
import qualified Syntax.Tree.Declaration as Syntax
import Verbose (Debug)

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

resolve ::
  forall scope verbose.
  (Debug verbose) =>
  Context scope ->
  Extensions ->
  (Int -> Term.Index scope) ->
  [Syntax.Declaration Position] ->
  verbose (Declarations scope)
bindings ::
  (Monoid stability) =>
  (Int -> Term0.Index scope) ->
  (Int -> Type0.Index scope) ->
  Declarations scope ->
  Bindings stability scope
shrink :: Declarations scope -> Real.Declarations locality Normal Resolve scope
