module Semantic.Unify.Instanciation where

import Control.Monad.ST (ST)
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Instanciation as Simple
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Scope (Environment (..))
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class (Solve, Zonk (..))
import {-# SOURCE #-} Semantic.Unify.Evidence (Evidence (..), Logical)
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence
import Syntax.Position (Position)

newtype Instanciation s scope = Instanciationx {runInstanciationx :: InstanciationF (Logical s) scope}

instance Shift (Instanciation s) where
  shift (Instanciationx instanciation) = Instanciationx $ shift instanciation

instance Zonk Instanciation where
  zonk zonker (Instanciationx instanciation) =
    Instanciationx <$> case instanciation of
      Instanciation instanciation -> do
        instanciation <- traverse (fmap runEvidencex . zonk zonker . Evidencex) instanciation
        pure $ Instanciation instanciation
      Mono -> pure Mono

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
