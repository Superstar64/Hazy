module Core.Substitute where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import {-# SOURCE #-} Core.Tree.Evidence (Evidence, EvidenceF)
import {-# SOURCE #-} Core.Tree.Type (Type, TypeF)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local, Vacuous)
import qualified Semantic.Shift as Shift
import Prelude hiding (Functor, map)

data Category typex evidence scope1 scope2 where
  Lift :: Shift2.Category scope1 scope2 -> Category typex evidence scope1 scope2
  Over ::
    Category typex evidence scopes scopes' ->
    Category typex evidence (scope1 ':+ scopes) (scope1 ':+ scopes')
  -- |
  -- Replace rigid evidence with new evidence. Note that rigid evidence cannot
  -- have an instanciation because quantified constraints are not supported.
  Substitute ::
    Shift.Category scope scope' ->
    Vector (typex scope') ->
    Vector (evidence scope') ->
    Category typex evidence (Local ':+ scope) scope'

general :: Category logicalType logicalEvidence scope1 scope2 -> Shift2.Category scope1 scope2
general = \case
  Lift category -> category
  Over category -> Shift2.Over (general category)
  Substitute general _ _ -> Shift2.Lift (general Shift.:. Shift.Unshift (error "bad general"))

class (Shift2.Functor typex) => Functor typex where
  map :: Category Type Evidence scope1 scope2 -> typex scope1 -> typex scope2

instance Functor Type.Index where
  map = Shift2.map . general

instance Functor Type2.Index where
  map = Shift2.map . general

instance Functor Constructor.Index where
  map = Shift2.map . general

instance Functor Selector.Index where
  map = Shift2.map . general

instance Functor Method.Index where
  map = Shift2.map . general

instance Functor Term.Index where
  map = Shift2.map . general

class TypeFunctor typef where
  mapType ::
    (Shift.Functor logical) =>
    Category (TypeF logical) vacuous scope1 scope2 ->
    typef Vacuous scope1 ->
    typef logical scope2

class EvidenceFunctor evidencef where
  mapEvidence ::
    (Shift.Functor logical) =>
    Category vacuous (EvidenceF logical) scope1 scope2 ->
    evidencef Vacuous scope1 ->
    evidencef logical scope2

mapInstances ::
  Category Type Evidence scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor typex) => Shift2.Category scope1 scope2 -> typex scope1 -> typex scope2
mapDefault = map . Lift
