module Core.Tree.Instanciation where

import qualified Core.Evidence.Functor as Core
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (EvidenceF)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Core.Type.Functor as Core.Type
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (null)

type Instanciation = InstanciationF Void Void

data InstanciationF logicalEvidence logicalType scope
  = Instanciation !(Strict.Vector (EvidenceF logicalEvidence logicalType scope))
  | Mono
      { levity :: !(TypeF logicalType scope)
      }
  deriving (Show)

instance
  (Show logicalEvidence, Show logicalType) =>
  Scope.Show (InstanciationF logicalEvidence logicalType)
  where
  showsPrec = showsPrec

instance Shift0.Functor (InstanciationF logicalEvidence logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (InstanciationF logicalEvidence logicalType) where
  map category = \case
    Instanciation instanciation -> Instanciation (Shift.map category <$> instanciation)
    Mono {levity} -> Mono {levity = Shift.map category levity}

instance
  (logicalEvidence ~ Void, logicalType ~ Void) =>
  Substitute.Functor (InstanciationF logicalEvidence logicalType)
  where
  map category = Substitute.mapEvidence category

instance Substitute.EvidenceFunctor InstanciationF where
  mapEvidence category = \case
    Instanciation instanciation ->
      Instanciation (Substitute.mapEvidence category <$> instanciation)
    Mono {levity} ->
      Mono
        { levity = Substitute.mapType (Substitute.anyEvidence category) levity
        }

instance Core.Functor InstanciationF where
  map f g = \case
    Instanciation instanciation ->
      Instanciation $ Core.map f g <$> instanciation
    Mono {levity} ->
      Mono
        { levity = Core.Type.map g levity
        }

monoLazy :: InstanciationF logicalEvidence logicalType scope
monoLazy = Mono {levity = Type.lazy}
