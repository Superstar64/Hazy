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

newtype Instanciation s scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence s scope)
  }

instance Zonk Instanciation where
  zonk zonker (Instanciation instanciation) = do
    instanciation <- traverse (zonk zonker) instanciation
    pure $ Instanciation instanciation

unify :: Instanciation s scope -> Instanciation s scope -> ST s ()
unify (Instanciation instanciation) (Instanciation instanciation') =
  sequence_ $ Strict.Vector.zipWith Evidence.unify instanciation instanciation'

unshift :: Instanciation s (scope ':+ scopes) -> ST s (Instanciation s scopes)
unshift (Instanciation instanciation) =
  Instanciation <$> traverse Evidence.unshift instanciation

solve :: Position -> Instanciation s scope -> Solve s (Simple.Instanciation scope)
solve position (Instanciation instanciation) = do
  instanciation <- traverse (Evidence.solve position) instanciation
  pure $ Simple.Instanciation instanciation
