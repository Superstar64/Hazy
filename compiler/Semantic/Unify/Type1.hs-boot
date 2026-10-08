module Semantic.Unify.Type1 where

import Control.Monad.ST (ST)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Index.Type2 as Type2
import {-# SOURCE #-} Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Type (Type)
import Syntax.Position (Position)

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrainWith ::
  forall s scope.
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  [Type s scope] ->
  ST s (Evidence s scope)
