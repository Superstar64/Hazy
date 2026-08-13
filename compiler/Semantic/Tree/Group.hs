module Semantic.Tree.Group where

import Core.Tree.Forall (ForallOver (..))
import qualified Core.Tree.Type as Simple (TypeF)
import qualified Core.Type.Functor as Core (Functor (..))
import qualified Core.Type.Show as Core (Show (..))
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Link.Term as Term (Link (..))
import Semantic.Layout (Layout)
import qualified Semantic.Layout as Layout
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Show as Term (Show (..))
import Semantic.Stage (Stage)
import Semantic.Tree.Combinators.Implicit (Implicit)
import Semantic.Tree.Combinators.Inferred (Inferred)
import qualified Semantic.Tree.Definition2 as Mark
import Semantic.Tree.Definition3 (Definition3)
import qualified Semantic.Unify as Unify

type Group :: (Type -> Type) -> Type -> Locality -> Layout -> Stage -> Environment -> Type
data Group solve logical locality layout stage scope
  = (::::)
      !(Inferred (ForallOver Types logical) stage scope)
      !(solve (Implicit (Set locality) Layout.Group stage scope))

infix 5 ::::

instance (Show logical, Show1 solve) => Show (Group solve logical locality layout stage scope) where
  showsPrec d (types :::: set) = showParen (d > 5) $ showsPrec 6 types . showString " :::: " . showsPrec1 6 set

instance (Functor solve) => Shift0.Functor (Group solve logical locality layout stage) where
  map = Shift.mapDefault

instance (Functor solve) => Shift.Functor (Group solve logical locality layout stage) where
  map category (types :::: set) = Shift.map category types :::: fmap (Shift.map category) set

instance (Foldable solve) => FreeTermVariables (Group solve logical locality) where
  freeTermVariables target (_ :::: set) = foldMap (freeTermVariables target) set

newtype Types logical scope = Types (Strict.Vector (Simple.TypeF logical scope))
  deriving (Show)

instance Core.Show Types where
  showsPrec = showsPrec

instance (Show logcial) => Scope.Show (Types logcial) where
  showsPrec = showsPrec

instance Shift0.Functor (Types logical) where
  map = Shift.mapDefault

instance Shift.Functor (Types logical) where
  map category (Types types) = Types (Shift.map category <$> types)

instance Unify.Zonk Types where
  zonk zonker category (Types types) = Types <$> traverse (Unify.zonk zonker category) types

instance Core.Functor Types where
  map f (Types types) = Types $ Core.map f <$> types

instance Unify.Generalizable Types where
  collect collector (Types types) = foldMap (Unify.collect collector) types

instance Unify.SolveType Types where
  solveWith category position (Types types) =
    Types <$> traverse (Unify.solveWith category position) types

newtype Set locality layout stage scope
  = Set (Strict.Vector (Element locality layout stage scope))
  deriving (Show)

instance Term.Show (Set locality) where
  showsPrec = showsPrec

instance Scope.Show (Set locality layout stage) where
  showsPrec = showsPrec

instance Shift0.Functor (Set locality layout stage) where
  map = Shift.mapDefault

instance Shift0.TermFunctor (Set locality) where
  mapTerm = Shift0.map

instance Shift.Functor (Set locality layout stage) where
  map category (Set set) = Set (Shift.map category <$> set)

instance Shift.TermFunctor (Set locality) where
  mapTerm = Shift.map

instance FreeTermVariables (Set locality) where
  freeTermVariables target (Set set) = foldMap (freeTermVariables target) set

data Element locality layout stage scope = Element
  { element :: !(Definition3 Mark.Inferred layout stage (Scope.GroupTerm ':+ scope)),
    link :: !(Term.Link locality)
  }
  deriving (Show)

instance Shift0.Functor (Element locality layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Element locality layout stage) where
  map category Element {element, link} =
    Element
      { element = Shift.map (Shift.Over category) element,
        link
      }

instance FreeTermVariables (Element locality) where
  freeTermVariables target Element {element} =
    freeTermVariables (FreeVariables.Over target) element
