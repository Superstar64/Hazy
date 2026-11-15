module Core.Tree.Instanciation where

import qualified Core.Evidence.Substitute as Evidence.Substitute
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (EvidenceF)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Core.Type.Substitute as Type.Substitute
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
  map = Evidence.Substitute.mapDefault

instance Evidence.Substitute.Functor InstanciationF where
  map category = \case
    Instanciation instanciation ->
      Instanciation (Evidence.Substitute.map category <$> instanciation)
    Mono {levity}
      | category <- Evidence.Substitute.typex category,
        category <- Type.Substitute.mapCategory Substitute.anyEvidence category ->
          Mono
            { levity = Type.Substitute.map category levity
            }

monoLazy :: InstanciationF logicalEvidence logicalType scope
monoLazy = Mono {levity = Type.lazy}
