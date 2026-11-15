module Core.Evidence.Substitute where

import Core.Substitute (Evidences, Types)
import qualified Core.Substitute as Substitute
import qualified Core.Type.Substitute as Type
import Data.Void (Void, absurd)
import Semantic.Scope (Singleton)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Functor, map)

data
  Category
    term
    types
    evidences
    scope1
    scope2
    logicalEvidence
    logicalEvidence'
    logicalType
    logicalType'
  = Category
  { mapEvidence :: logicalEvidence -> logicalEvidence',
    mapType :: logicalType -> logicalType',
    category :: !(Substitute.Category term types evidences scope1 scope2)
  }

mapCategory ::
  ( Substitute.Category term1 types1 evidences1 scope1 scope1' ->
    Substitute.Category term2 types2 evidences2 scope2 scope2'
  ) ->
  Category
    term1
    types1
    evidences1
    scope1
    scope1'
    logicalEvidence
    logicalEvidence'
    logicalType
    logicalType' ->
  Category
    term2
    types2
    evidences2
    scope2
    scope2'
    logicalEvidence
    logicalEvidence'
    logicalType
    logicalType'
mapCategory f Category {mapEvidence, mapType, category} =
  Category
    { mapEvidence,
      mapType,
      category = f category
    }

lift ::
  Substitute.Category term types evidences scope1 scope2 ->
  Category
    term
    types
    evidences
    scope1
    scope2
    logicalEvidence
    logicalEvidence
    logicalType
    logicalType
lift category = Category {mapType = id, mapEvidence = id, category}

general ::
  Category
    term
    types
    evidences
    scope1
    scope2
    logicalEvidence
    logicalEvidence'
    logicalType
    logicalType' ->
  Shift.Category scope1 scope2
general Category {category} = Substitute.general category

typex ::
  Category
    term
    types
    evidences
    scope1
    scope2
    logicalEvidence
    logicalEvidence'
    logicalType
    logicalType' ->
  Type.Category
    term
    types
    evidences
    scope1
    scope2
    logicalType
    logicalType'
typex Category {mapType, category} = Type.Category {mapType, category}

class Functor evidencef where
  map ::
    Category
      Singleton
      (Types logicalType')
      (Evidences logicalEvidence' logicalType')
      scope1
      scope2
      logicalEvidence
      logicalEvidence'
      logicalType
      logicalType' ->
    evidencef logicalEvidence logicalType scope1 ->
    evidencef logicalEvidence' logicalType' scope2

shift ::
  (Functor evidencef, Shift0.Functor f1, Shift0.Functor f2) =>
  Shift0.Category scope1 scope2 ->
  evidencef (f1 scope1) (f2 scope1) scope1 ->
  evidencef (f1 scope2) (f2 scope2) scope2
shift category =
  map
    Category
      { mapType = Shift0.map category,
        mapEvidence = Shift0.map category,
        category = Substitute.Lift (Shift.lift category)
      }

logical ::
  (Functor evidencef) =>
  evidencef Void Void scope ->
  evidencef logicalEvidence logicalType scope
logical =
  map
    Category
      { mapType = absurd,
        mapEvidence = absurd,
        category = Substitute.Lift Shift.Id
      }

mapDefault ::
  (Functor evidencef) =>
  Substitute.Category
    term
    (Types logicalType')
    (Evidences logicalEvidence' logicalType')
    scope1
    scope2 ->
  evidencef logicalEvidence' logicalType' scope1 ->
  evidencef logicalEvidence' logicalType' scope2
mapDefault category = map $ lift $ Substitute.anyTerm category
