module Stage4.Tree.Instanciation (Instanciation (..), null, empty) where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Evidence (Evidence)
import Prelude hiding (null)
import qualified Prelude

newtype Instanciation scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence scope)
  }
  deriving (Show)

instance Shift Instanciation where
  shift = shiftDefault

instance Shift.Functor Instanciation where
  map = Shift2.mapDefault

instance Shift2.Functor Instanciation where
  map = Substitute.mapDefault

instance Substitute.Functor Instanciation where
  map category (Instanciation instanciation) =
    Instanciation (Substitute.map category <$> instanciation)

null (Instanciation evidence) = Prelude.null evidence

empty :: Instanciation scope
empty = Instanciation Strict.Vector.empty
