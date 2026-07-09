module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (EvidenceF)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (IsVacuous, Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Prelude hiding (null)

type Instanciation = InstanciationF Vacuous

data InstanciationF logical scope
  = Instanciation !(Strict.Vector (EvidenceF logical scope))
  | Mono
  deriving (Show)

instance (Scope.Show logical) => Scope.Show (InstanciationF logical) where
  showsPrec = showsPrec

instance (Shift.Functor logical) => Shift (InstanciationF logical) where
  shift = shiftDefault

instance (Shift.Functor logical) => Shift.Functor (InstanciationF logical) where
  map category = \case
    Instanciation instanciation ->
      Instanciation (Shift.map category <$> instanciation)
    Mono -> Mono

instance (IsVacuous logical, Shift.Functor logical) => Shift2.Functor (InstanciationF logical) where
  map = Substitute.mapDefault

instance (IsVacuous logical, Shift.Functor logical) => Substitute.Functor (InstanciationF logical) where
  map category = \case
    Instanciation instanciation ->
      Instanciation (Substitute.map category <$> instanciation)
    Mono -> Mono
