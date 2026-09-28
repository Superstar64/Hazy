module Core.Substitute where

import {-# SOURCE #-} Core.Tree.Evidence (Evidence, EvidenceF)
import {-# SOURCE #-} Core.Tree.Type (Type, TypeF)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Functor, map)

data Category typex evidence scope1 scope2 where
  Lift :: Shift.Category scope1 scope2 -> Category typex evidence scope1 scope2
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

general :: Category logicalType logicalEvidence scope1 scope2 -> Shift.Category scope1 scope2
general = \case
  Lift category -> category
  Over category -> Shift.Over (general category)
  Substitute general _ _ -> general Shift.:. Shift.Unshift (error "bad general")

class (Shift.Functor typex) => Functor typex where
  map :: Category Type Evidence scope1 scope2 -> typex scope1 -> typex scope2

instance Functor Type2.Index where
  map = Shift.map . general

instance Functor Constructor.Index where
  map = Shift.map . general

instance Functor Selector.Index where
  map = Shift.map . general

instance Functor Method.Index where
  map = Shift.map . general

instance Functor Term.Index where
  map = Shift.map . general

class TypeFunctor typef where
  mapType ::
    Category (TypeF logical) vacuous scope1 scope2 ->
    typef Void scope1 ->
    typef logical scope2

substituteType ::
  (TypeFunctor typef) =>
  Vector (TypeF logical scope2) ->
  typef Void (Local ':+ scope2) ->
  typef logical scope2
substituteType substitution typex = mapType substitute typex
  where
    substitute = Substitute Shift.Id substitution Vector.empty

logicalType ::
  (TypeFunctor typef, Shift0.Functor (typef Void)) =>
  typef Void scope2 -> typef logical scope2
logicalType = substituteType Vector.empty . shift

class EvidenceFunctor evidencef where
  mapEvidence ::
    Category vacuous (EvidenceF logical) scope1 scope2 ->
    evidencef Void scope1 ->
    evidencef logical scope2

substituteEvidence ::
  (EvidenceFunctor evidencef) =>
  Vector (EvidenceF logical scope2) ->
  evidencef Void (Local ':+ scope2) ->
  evidencef logical scope2
substituteEvidence substitution evidence = mapEvidence substitute evidence
  where
    substitute = Substitute Shift.Id Vector.empty substitution

logicalEvidence ::
  (EvidenceFunctor evidencef, Shift0.Functor (evidencef Void)) =>
  evidencef Void scope2 -> evidencef logical scope2
logicalEvidence = substituteEvidence Vector.empty . shift

mapInstances ::
  Category Type Evidence scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor typex) => Shift.Category scope1 scope2 -> typex scope1 -> typex scope2
mapDefault = map . Lift
