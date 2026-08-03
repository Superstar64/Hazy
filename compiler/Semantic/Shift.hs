module Semantic.Shift (module Semantic.Shift, shift) where

import Data.Kind (Constraint, Type)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict
import Data.Void (Void, absurd, vacuous)
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Term0 as Term0
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type0 as Type0
import {-# SOURCE #-} Semantic.Index.Type2 as Type2 (Index)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment ((:+)), Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)
import Prelude hiding (Functor, id, map, (.))

data Category scope scope' where
  -- Basic shifts
  Id :: Category scope scope
  Shift :: Category scopes (scope ':+ scopes)
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  (:.) :: Category scope' scope'' -> Category scope scope' -> Category scope scope''
  -- Semantic shifs
  Unshift :: Void -> Category (scope ':+ scopes) scopes
  GroupTerm ::
    (Term0.Index scope -> Strict.Maybe Int) ->
    Category scope (Scope.GroupTerm ':+ scope)
  GroupType ::
    (Type0.Index scope -> Strict.Maybe Int) ->
    Category scope (Scope.GroupType ':+ scope)
  UngroupTerm ::
    (Int -> Term.Index scope) ->
    Category (Scope.GroupTerm ':+ scope) scope
  UngroupType ::
    (Int -> Type.Index scope) ->
    Category (Scope.GroupType ':+ scope) scope
  -- Core shifts
  ReplaceWildcard ::
    Category
      (Scope.Pattern ':+ scope)
      (Scope.SimpleDeclaration ':+ scope)
  SimplifyPattern ::
    Int ->
    Category
      (Scope.Pattern ':+ scope)
      (Scope.Pattern ':+ Scope.Pattern ':+ scope)
  RenamePattern ::
    (Int -> Int) ->
    Category
      (Scope.Pattern ':+ scope)
      (Scope.Pattern ':+ scope)
  SimplifyList ::
    Category
      (Scope.Pattern ':+ scope)
      (Scope.Pattern ':+ scope)
  LetPattern ::
    Category
      (Scope.Pattern ':+ scope)
      (Scope.Pattern ':+ Scope.SimpleDeclaration ':+ scope)
  FinishPattern ::
    Category
      (Scope.Pattern ':+ scope)
      (Scope.SimplePattern ':+ scope)
  FinishNewtype ::
    Category
      (Scope.SimplePattern ':+ scope)
      (Scope.SimpleDeclaration ':+ scope)
  ReplaceIrrefutable ::
    Int ->
    Category
      (Scope.SimplePattern ':+ scope)
      (Scope.SimplePattern ':+ (Scope.SimpleDeclaration ':+ scope))

infixr 9 :.

generalReplaceWildcard = Unshift (error "bad unshift") :. Over Shift

generalSimplifyPattern = Shift

generalRenamePattern = Id

generalSimplifyList = Id

generalLetPattern = Over Shift

generalFinishPattern = Unshift (error "bad unshift") :. Over Shift

generalFinishNewtype = Unshift (error "bad unshift") :. Over Shift

generalReplaceIrrefutable = Over Shift

class (Shift0.Functor f) => Functor f where
  map :: Category scope scope' -> f scope -> f scope'

instance Functor Term.Index where
  map Id index = index
  map Shift index = Term.Shift index
  map (Over category) (Term.Shift index) = Term.Shift $ map category index
  map (Over _) (Term.Declaration index) = Term.Declaration index
  map (Over _) (Term.Pattern bound) = Term.Pattern bound
  map (Over _) (Term.Group index) = Term.Group index
  map (Over _) (Term.SimplePattern index) = Term.SimplePattern index
  map (Over _) Term.SimpleDeclaration = Term.SimpleDeclaration
  map (after :. before) index = map after (map before index)
  map (Unshift _) (Term.Shift index) = index
  map (Unshift abort) _ = absurd abort
  map (GroupTerm term) (Term.Declaration index)
    | Strict.Just index <- term (Term0.Declaration index) = Term.Group index
  map (GroupTerm term) (Term.Global global local)
    | Strict.Just index <- term (Term0.Global global local) = Term.Group index
  map GroupTerm {} index = Term.Shift index
  map GroupType {} index = Term.Shift index
  map (UngroupTerm term) (Term.Group index) = term index
  map UngroupTerm {} (Term.Shift index) = index
  map UngroupType {} (Term.Shift index) = index
  map ReplaceWildcard index = case index of
    Term.Pattern Term.At -> Term.SimpleDeclaration
    Term.Pattern _ -> error "bad wildcard bind"
    Term.Shift index -> Term.Shift index
  map (SimplifyPattern target) index = case index of
    Term.Pattern (Term.Select source bound)
      | source == target -> Term.Pattern bound
    Term.Pattern _ -> shift index
    Term.Shift _ -> shift index
  map (RenamePattern rename) index = case index of
    Term.Pattern Term.At -> Term.Pattern Term.At
    Term.Pattern (Term.Select index' bound) ->
      Term.Pattern (Term.Select (rename index') bound)
    Term.Shift index -> Term.Shift index
  map SimplifyList index = case index of
    Term.Pattern Term.At -> Term.Pattern Term.At
    Term.Pattern (Term.Select 0 bound) -> Term.Pattern (Term.Select 0 bound)
    Term.Pattern (Term.Select index bound) ->
      Term.Pattern (Term.Select 1 (Term.Select (index - 1) bound))
    Term.Shift index -> Term.Shift index
  map LetPattern index = case index of
    Term.Pattern Term.At -> Term.Shift Term.SimpleDeclaration
    Term.Pattern (Term.Select index bound) ->
      Term.Pattern (Term.Select index bound)
    Term.Shift index -> Term.Shift (Term.Shift index)
  map FinishPattern index = case index of
    Term.Pattern (Term.Select index Term.At) -> Term.SimplePattern index
    Term.Pattern _ -> error "bad finish pattern"
    Term.Shift index -> Term.Shift index
  map FinishNewtype index = case index of
    Term.SimplePattern n
      | n == 0 -> Term.SimpleDeclaration
      | otherwise -> error "bad finish newtype"
    Term.Shift index -> Term.Shift index
  map (ReplaceIrrefutable target) index = case index of
    Term.SimplePattern index
      | index == target -> Term.Shift Term.SimpleDeclaration
      | otherwise -> Term.SimplePattern index
    Term.Shift index -> Term.Shift (Term.Shift index)

instance Functor Type.Index where
  map Id index = index
  map Shift index = Type.Shift index
  map (Over category) (Type.Shift index) = Type.Shift (map category index)
  map (Over _) (Type.Declaration index) = Type.Declaration index
  map (Over _) (Type.Group index) = Type.Group index
  map (after :. before) index = map after (map before index)
  map (Unshift _) (Type.Shift index) = index
  map (Unshift abort) _ = absurd abort
  map (GroupType typex) (Type.Declaration index)
    | Strict.Just index <- typex (Type0.Declaration index) = Type.Group index
  map (GroupType typex) (Type.Global global local)
    | Strict.Just index <- typex (Type0.Global global local) = Type.Group index
  map GroupTerm {} index = Type.Shift index
  map GroupType {} index = Type.Shift index
  map (UngroupType typex) (Type.Group index) = typex index
  map UngroupType {} (Type.Shift index) = index
  map UngroupTerm {} (Type.Shift index) = index
  map category index = map general index
    where
      general = case category of
        ReplaceWildcard {} -> generalReplaceWildcard
        SimplifyPattern {} -> generalSimplifyPattern
        RenamePattern {} -> generalRenamePattern
        SimplifyList {} -> generalSimplifyList
        LetPattern {} -> generalLetPattern
        FinishPattern {} -> generalFinishPattern
        FinishNewtype {} -> generalFinishNewtype
        ReplaceIrrefutable {} -> generalReplaceIrrefutable

instance PartialUnshift Type.Index where
  partialUnshift _ (Type.Shift index) = pure index
  partialUnshift abort _ = vacuous abort

instance Functor Evidence0.Index where
  map Id index = index
  map (Over _) (Evidence0.Assumed index) = Evidence0.Assumed index
  map (Over category) (Evidence0.Shift index) = Evidence0.Shift (map category index)
  map Shift index = Evidence0.Shift index
  map (after :. before) index = map after $ map before index
  map (Unshift _) (Evidence0.Shift index) = index
  map (Unshift abort) Evidence0.Assumed {} = absurd abort
  map GroupTerm {} index = Evidence0.Shift index
  map GroupType {} index = Evidence0.Shift index
  map UngroupTerm {} (Evidence0.Shift index) = index
  map UngroupType {} (Evidence0.Shift index) = index
  map category index = map general index
    where
      general = case category of
        ReplaceWildcard {} -> generalReplaceWildcard
        SimplifyPattern {} -> generalSimplifyPattern
        RenamePattern {} -> generalRenamePattern
        SimplifyList {} -> generalSimplifyList
        LetPattern {} -> generalLetPattern
        FinishPattern {} -> generalFinishPattern
        FinishNewtype {} -> generalFinishNewtype
        ReplaceIrrefutable {} -> generalReplaceIrrefutable

instance PartialUnshift Evidence0.Index where
  partialUnshift abort (Evidence0.Assumed _) = vacuous abort
  partialUnshift _ (Evidence0.Shift index) = pure index

instance Functor Local.Index where
  map Id index = index
  map Shift index = Local.Shift index
  map (Over category) (Local.Shift index) = Local.Shift $ map category index
  map (Over _) (Local.Local index) = Local.Local index
  map (after :. before) index = map after (map before index)
  map (Unshift _) (Local.Shift index) = index
  map (Unshift abort) Local.Local {} = absurd abort
  map (GroupTerm _) index = Local.Shift index
  map (GroupType _) index = Local.Shift index
  map UngroupTerm {} (Local.Shift index) = index
  map UngroupType {} (Local.Shift index) = index
  map category index = map general index
    where
      general = case category of
        ReplaceWildcard {} -> generalReplaceWildcard
        SimplifyPattern {} -> generalSimplifyPattern
        RenamePattern {} -> generalRenamePattern
        SimplifyList {} -> generalSimplifyList
        LetPattern {} -> generalLetPattern
        FinishPattern {} -> generalFinishPattern
        FinishNewtype {} -> generalFinishNewtype
        ReplaceIrrefutable {} -> generalReplaceIrrefutable

instance PartialUnshift Local.Index where
  partialUnshift _ (Local.Shift index) = pure index
  partialUnshift abort Local.Local {} = vacuous abort

instance Functor Vacuous where
  map _ = \case {}

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances GroupType {} =
  error "group type shifts are not monotonic"
mapInstances UngroupType {} =
  error "group type shifts are not monotonic"
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor f) => Shift0.Category scope scope' -> f scope -> f scope'
mapDefault category = map (lift category) where

lift :: Shift0.Category scope scope' -> Category scope scope'
lift = \case
  Shift0.Id -> Id
  Shift0.Shift -> Shift
  after Shift0.:. before -> lift after :. lift before

class PartialUnshift f where
  partialUnshift :: (Applicative m) => m Void -> f (scope ':+ scopes) -> m (f scopes)

class Unshift f where
  unshift :: f (scope ':+ scopes) -> f scopes

type TermFunctor :: (Layout -> Stage -> Environment -> Type) -> Constraint
class (Shift0.TermFunctor term) => TermFunctor term where
  mapTerm :: Category scope scope' -> term layout stage scope -> term layout stage scope'
