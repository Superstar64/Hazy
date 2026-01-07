module Stage3.Simple.Instanciation where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Simple.Evidence (Evidence)

newtype Instanciation scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence scope)
  }
  deriving (Show)

instance Shift Instanciation where
  shift = shiftDefault

instance Shift.Functor Instanciation where
  map category (Instanciation instanciation) =
    Instanciation (Shift.map category <$> instanciation)
