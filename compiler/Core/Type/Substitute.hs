module Core.Type.Substitute where

import Core.Substitute (Types)
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Type (TypeF)
import qualified Data.Vector.Strict as Strict
import Data.Void (Void, absurd)
import Semantic.Scope (Environment (..), Local, Singleton (..))
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
    logicalType
    logicalType'
  = Category
  { mapType :: logicalType -> logicalType',
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
    logicalType
    logicalType' ->
  Category
    term2
    types2
    evidences2
    scope2
    scope2'
    logicalType
    logicalType'
mapCategory f Category {mapType, category} =
  Category
    { mapType,
      category = f category
    }

over ::
  Category
    term
    types
    evidences
    scopes
    scopes'
    logicalType
    logicalType' ->
  Category
    term
    types
    evidences
    (scopex ':+ scopes)
    (scopex ':+ scopes')
    logicalType
    logicalType'
over = mapCategory Substitute.Over

lift ::
  Substitute.Category term types evidences scope1 scope2 ->
  Category term types evidences scope1 scope2 logicalType' logicalType'
lift category = Category {mapType = id, category}

general ::
  Category
    term
    types
    evidences
    scope1
    scope2
    logicalType
    logicalType' ->
  Shift.Category scope1 scope2
general Category {category} = Substitute.general category

class Functor typef where
  map ::
    Category
      Singleton
      (Types logicalType')
      Singleton
      scope1
      scope2
      logicalType
      logicalType' ->
    typef logicalType scope1 ->
    typef logicalType' scope2

substitute ::
  (Functor typef) =>
  Strict.Vector (TypeF logicalType' scope2) ->
  typef Void (Local ':+ scope2) ->
  typef logicalType' scope2
substitute types =
  map
    Category
      { mapType = absurd,
        category = Substitute.SubstituteLocal Shift.Id (Substitute.Types types) Singleton
      }

shift ::
  (Functor typef, Shift0.Functor f) =>
  typef (f scope) scope ->
  typef (f (scopex ':+ scope)) (scopex ':+ scope)
shift = shiftWith Shift0.Shift

shiftWith ::
  (Functor typef, Shift0.Functor f) =>
  Shift0.Category scope1 scope2 ->
  typef (f scope1) scope1 ->
  typef (f scope2) scope2
shiftWith category =
  map
    Category
      { mapType = Shift0.map category,
        category = Substitute.Lift (Shift.lift category)
      }

logical :: (Functor typef) => typef Void scope2 -> typef logicalType scope2
logical =
  map
    Category
      { mapType = absurd,
        category = Substitute.Lift Shift.Id
      }

mapDefault ::
  (Functor typef) =>
  Substitute.Category
    term
    (Types logicalType)
    evidences
    scope1
    scope2 ->
  typef logicalType scope1 ->
  typef logicalType scope2
mapDefault category = map $ lift $ Substitute.anyEvidence $ Substitute.anyTerm category
