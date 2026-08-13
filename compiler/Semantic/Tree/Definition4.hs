module Semantic.Tree.Definition4 where

import Core.Tree.TypeLambda (TypeLambdaOver (..))
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.Connect (connect)
import qualified Semantic.Connect as Connect
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.Index.Link.Term as Term (Link (..))
import qualified Semantic.Index.Term0 as Term0
import Semantic.Layout (Layout, Normal)
import qualified Semantic.Layout as Layout
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Tree.Combinators.Implicit (Implicit)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import {-# SOURCE #-} Semantic.Tree.Declaration (Groupable (..))
import qualified Semantic.Tree.Definition2 as Mark
import Semantic.Tree.Definition3 (Definition3)
import Semantic.Tree.Group (Element (..), Group (..), Set (..))
import Semantic.Tree.Scheme (Scheme)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type Definition4 :: (Type -> Type) -> Type -> Locality -> Layout -> Stage -> Environment -> Type
data Definition4 solve logical locality layout stage scope where
  (:::) ::
    !(Annotation mark layout stage scope) ->
    !(solve (Implicit (Definition3 mark) layout stage scope)) ->
    Definition4 solve logical locality layout stage scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 solve logical locality Layout.Group stage scope
  Group ::
    !(Group solve logical locality Layout.Group stage scope) ->
    Definition4 solve logical locality Layout.Group stage scope

infix 5 :::

instance (Show1 solve, Show logical) => Show (Definition4 solve logical locality layout stage scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 5) $
      showsPrec 6 annotation . showString " ::: " . showsPrec1 6 definition
  showsPrec d (Link link id) =
    showParen (d > 10) $
      showString "Link "
        . showsPrec 11 link
        . showString " "
        . showsPrec 11 id
  showsPrec d (Group group) =
    showParen (d > 10) $
      showString "Group " . showsPrec 11 group

instance (Functor solve) => Shift0.Functor (Definition4 solve logical locality layout stage) where
  map = Shift.mapDefault

instance (Functor solve) => Shift.Functor (Definition4 solve logical locality layout stage) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: fmap (Shift.map category) definition
    Link link id -> Link link id
    Group group -> Group (Shift.map category group)

instance (Foldable solve) => FreeTermVariables (Definition4 solve logical locality) where
  freeTermVariables target = \case
    _ ::: definition -> foldMap (freeTermVariables target) definition
    Link {} -> []
    Group group -> freeTermVariables target group

data Annotation mark layout stage scope where
  Annotated :: !(Scheme Position stage scope) -> Annotation Mark.Annotated layout stage scope
  Inferred :: Annotation Mark.Inferred Normal stage scope

instance Shift0.Functor (Annotation mark layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Annotation mark layout stage) where
  map category = \case
    Annotated scheme -> Annotated (Shift.map category scheme)
    Inferred -> Inferred

instance Show (Annotation mark scope layout stage) where
  showsPrec d annotation = case annotation of
    Annotated scheme -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 scheme
    Inferred -> showString "Inferred"

locality ::
  Definition4 solve logical locality Normal stage scope ->
  Definition4 solve logical locality' Normal stage scope
locality = \case
  annotation ::: declaration -> annotation ::: declaration

group ::
  (Term0.Index scope -> Term.Link locality) ->
  (Term.Link locality -> Groupable scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Definition4 Identity logical locality Normal Resolve scope ->
  Definition4 Identity logical locality Layout.Group Resolve scope
group _ _ _ (Annotated annotation ::: Identity (Implicit.Resolve definition)) =
  Annotated annotation ::: Identity (Implicit.Resolve (connect definition))
group link index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group (Inferred.Inferred :::: Identity (Implicit.Resolve (Set $ Strict.Vector.fromList $ map go $ Set.toList set)))
    where
      go link = case index link of
        Groupable {element} ->
          Element
            { element = Shift.map (Shift.GroupTerm lookup) $ connect element,
              link
            }
      lookup index = Strict.Maybe.fromLazy $ Set.lookupIndex (link index) set
  StronglyConnected.Link {link, id} -> Link link id

ungroup ::
  (Term.Link locality -> Term0.Index scope) ->
  (Term.Link locality -> Implicit (Set locality) Layout.Group Check scope) ->
  Definition4 Identity logical locality Layout.Group Check scope ->
  Definition4 Identity logical locality Normal Check scope
ungroup _ _ (Annotated annotation ::: Identity (Implicit.Check definition)) =
  Annotated annotation ::: Identity (Implicit.Check (TypeLambda.map (TypeLambda.Map Connect.seperate) definition))
ungroup index lookup definition = case definition of
  Link index id -> Inferred ::: Identity (go id (lookup index))
  Group (_ :::: Identity set) -> Inferred ::: Identity (go 0 set)
  where
    go id (Implicit.Check TypeLambdaOver {parameters, constraints, result = Set set}) =
      Implicit.Check $
        TypeLambdaOver
          { parameters,
            constraints,
            result = Connect.seperate $ Shift.map (Shift.UngroupTerm original) element
          }
      where
        original id
          | Element {link} <- set Strict.Vector.! id =
              shift $ Term0.normal $ index link
        Element {element} = set Strict.Vector.! id

solve ::
  Position ->
  Definition4 (Unify.Solve s) (Unify.Logical s scope) locality Layout.Group Check scope ->
  Unify.Solve s (Definition4 Identity Void locality Layout.Group Check scope)
solve position = \case
  (annotation ::: definition) -> do
    definition <- definition
    pure $ annotation ::: pure definition
  Link link id -> pure (Link link id)
  Group (Inferred.Solved types :::: set) -> do
    types <- Unify.solve position types
    set <- set
    pure $ Group (Inferred.Solved types :::: pure set)
