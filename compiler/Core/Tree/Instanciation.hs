module Core.Tree.Instanciation (Instanciation (..), null, empty) where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (Evidence)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Prelude hiding (null)
import qualified Prelude

newtype Instanciation scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence scope)
  }
  deriving (Show)

instance Scope.Show Instanciation where
  showsPrec = showsPrec

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
