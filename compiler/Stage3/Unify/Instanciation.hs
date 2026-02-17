module Stage3.Unify.Instanciation where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage3.Unify.Class (Zonk (..))
import Stage3.Unify.Evidence (Evidence)
import qualified Stage3.Unify.Evidence as Evidence
import qualified Stage4.Tree.Instanciation as Simple

newtype Instanciation s scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence s scope)
  }

instance Zonk Instanciation where
  zonk zonker (Instanciation instanciation) = do
    instanciation <- traverse (zonk zonker) instanciation
    pure $ Instanciation instanciation

solve :: Position -> Instanciation s scope -> ST s (Simple.Instanciation scope)
solve position (Instanciation instanciation) = do
  instanciation <- traverse (Evidence.solve position) instanciation
  pure $ Simple.Instanciation instanciation
