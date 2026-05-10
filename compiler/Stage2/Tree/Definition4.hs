module Stage2.Tree.Definition4 where

import Data.Kind (Type)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage2.Connect (connect)
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Term0 as Term0
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Inferred)
import qualified Stage2.Tree.Definition2 as Mark
import Stage2.Tree.Definition3 (Definition3)
import Stage2.Tree.Scheme (Scheme)

type Definition4 :: Locality -> Layout -> Environment -> Type
data Definition4 locality layout scope where
  (:::) :: !(Annotation mark layout scope) -> !(Definition3 mark layout scope) -> Definition4 locality layout scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality Group scope
  Group :: !(Strict.Vector (Definition3 Inferred Group (Scope.Group ':+ scope))) -> Definition4 locality Group scope

infixr 5 :::

instance Show (Definition4 locality layout scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 5) $
      showsPrec 6 annotation . showString " ::: " . showsPrec 6 definition
  showsPrec d (Link link id) =
    showParen (d > 10) $
      showString "Link "
        . showsPrec 11 link
        . showString " "
        . showsPrec 11 id
  showsPrec d (Group set) = showParen (d > 10) $ showString "Group " . showsPrec 11 set

instance Shift (Definition4 locality layout) where
  shift = shiftDefault

instance Shift.Functor (Definition4 locality layout) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: Shift.map category definition
    Link link id -> Link link id
    Group set -> Group (Shift.map (Shift.Over category) <$> set)

instance FreeTermVariables (Definition4 locality layout) where
  freeTermVariables target = \case
    _ ::: definition -> freeTermVariables target definition
    Link {} -> []
    Group set -> foldMap (freeTermVariables (FreeVariables.Over target)) set

data Annotation mark layout scope where
  Annotated :: !(Scheme Position scope) -> Annotation Mark.Annotated layout scope
  Inferred :: Annotation Mark.Inferred Normal scope

instance Shift (Annotation mark layout) where
  shift = shiftDefault

instance Shift.Functor (Annotation mark layout) where
  map category = \case
    Annotated scheme -> Annotated (Shift.map category scheme)
    Inferred -> Inferred

instance Show (Annotation mark scope layout) where
  showsPrec d annotation = case annotation of
    Annotated scheme -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 scheme
    Inferred -> showString "Inferred"

locality :: Definition4 locality Normal scope -> Definition4 locality' Normal scope
locality = \case
  annotation ::: declaration -> annotation ::: declaration

group ::
  (Term0.Index scope -> Term.Link locality) ->
  (Term.Link locality -> Definition3 Inferred Normal scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Definition4 locality Normal scope ->
  Definition4 locality Group scope
group _ _ _ (Annotated annotation ::: definition) = Annotated annotation ::: connect definition
group link index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group $ Strict.Vector.fromList $ map (transform . index) $ Set.toList set
    where
      transform = Shift.map (Shift.GroupTerm lookup) . connect
      lookup index = Strict.Maybe.fromLazy $ Set.lookupIndex (link index) set
  StronglyConnected.Link {link, id} -> Link link id
