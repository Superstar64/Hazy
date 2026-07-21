module Semantic.Unify.Instanciation where

import Control.Monad.ST (ST)
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Instanciation as Simple
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Scope (Environment (..))
import {-# SOURCE #-} Semantic.Unify.Evidence (Logical)
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence
import Semantic.Unify.Solve (Solve)
import Syntax.Position (Position)

type Instanciation s = InstanciationF (Logical s)

unify :: InstanciationF (Logical s) scope -> InstanciationF (Logical s) scope -> ST s ()
unify (Instanciation instanciation) (Instanciation instanciation') =
  sequence_ $ Strict.Vector.zipWith Evidence.unify instanciation instanciation'
unify Mono Mono = pure ()
unify _ _ = error "unify instanciation can't fail"

unshift :: InstanciationF (Logical s) (scope ':+ scopes) -> ST s (InstanciationF (Logical s) scopes)
unshift = \case
  Instanciation instanciation ->
    Instanciation <$> traverse Evidence.unshift instanciation
  Mono -> pure Mono

solve :: Position -> InstanciationF (Logical s) scope -> Solve s (Simple.Instanciation scope)
solve position = \case
  Instanciation instanciation -> do
    instanciation <- traverse (Evidence.solve position) instanciation
    pure $ Simple.Instanciation instanciation
  Mono -> pure Simple.Mono
