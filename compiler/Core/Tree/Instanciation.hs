module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (EvidenceF)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (null)

type Instanciation = InstanciationF Vacuous

data InstanciationF logical scope
  = Instanciation !(Strict.Vector (EvidenceF logical scope))
  | Mono
  deriving (Show)

instance (Scope.Show logical) => Scope.Show (InstanciationF logical) where
  showsPrec = showsPrec

instance (Shift.Functor logical) => Shift0.Functor (InstanciationF logical) where
  map = Shift.mapDefault

instance (Shift.Functor logical) => Shift.Functor (InstanciationF logical) where
  map category = \case
    Instanciation instanciation ->
      Instanciation (Shift.map category <$> instanciation)
    Mono -> Mono

instance (logical ~ Vacuous) => Shift2.Functor (InstanciationF logical) where
  map = Substitute.mapDefault

instance (logical ~ Vacuous) => Substitute.Functor (InstanciationF logical) where
  map = Substitute.mapEvidence

instance Substitute.EvidenceFunctor InstanciationF where
  mapEvidence category = \case
    Instanciation instanciation ->
      Instanciation (Substitute.mapEvidence category <$> instanciation)
    Mono -> Mono
