{-# LANGUAGE RoleAnnotations #-}

module Stage2.Temporary.Complete.Declarations where

import Data.Kind (Type)
import Stage1.Extensions (Extensions)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1
import qualified Stage2.Index.Term as Term
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Index.Type0 as Type0
import Stage2.Layout (Normal)
import Stage2.Resolve.Bindings (Bindings)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Real
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
  [Stage1.Declaration Position] ->
  verbose (Declarations scope)
bindings ::
  (Monoid stability) =>
  (Int -> Term0.Index scope) ->
  (Int -> Type0.Index scope) ->
  Declarations scope ->
  Bindings stability scope
shrink :: Declarations scope -> Real.Declarations locality Normal scope
