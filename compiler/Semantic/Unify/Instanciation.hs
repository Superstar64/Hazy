module Semantic.Unify.Instanciation where

import Control.Monad.ST (ST)
import qualified Core.Tree.Instanciation as Simple
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Scope (Environment (..))
import Semantic.Unify.Class (Solve, Zonk (..))
import {-# SOURCE #-} Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence
import Syntax.Position (Position)

data Instanciation s scope
  = Instanciation !(Strict.Vector (Evidence s scope))
  | Mono

instance Zonk Instanciation where
  zonk zonker = \case
    Instanciation instanciation -> do
      instanciation <- traverse (zonk zonker) instanciation
      pure $ Instanciation instanciation
    Mono -> pure Mono

unify :: Instanciation s scope -> Instanciation s scope -> ST s ()
unify (Instanciation instanciation) (Instanciation instanciation') =
  sequence_ $ Strict.Vector.zipWith Evidence.unify instanciation instanciation'
unify Mono Mono = pure ()
unify _ _ = error "unify instanciation can't fail"

unshift :: Instanciation s (scope ':+ scopes) -> ST s (Instanciation s scopes)
unshift = \case
  Instanciation instanciation ->
    Instanciation <$> traverse Evidence.unshift instanciation
  Mono -> pure Mono

solve :: Position -> Instanciation s scope -> Solve s (Simple.Instanciation scope)
solve position = \case
  Instanciation instanciation -> do
    instanciation <- traverse (Evidence.solve position) instanciation
    pure $ Simple.Instanciation instanciation
  Mono -> pure Simple.Mono
