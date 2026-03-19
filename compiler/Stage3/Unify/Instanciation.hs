module Stage3.Unify.Instanciation where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage2.Scope (Environment (..))
import Stage3.Unify.Class (Zonk (..))
import {-# SOURCE #-} Stage3.Unify.Evidence (Evidence)
import {-# SOURCE #-} qualified Stage3.Unify.Evidence as Evidence
import qualified Stage4.Tree.Instanciation as Simple

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

solve :: Position -> Instanciation s scope -> ST s (Simple.Instanciation scope)
solve position (Instanciation instanciation) = do
  instanciation <- traverse (Evidence.solve position) instanciation
  pure $ Simple.Instanciation instanciation
