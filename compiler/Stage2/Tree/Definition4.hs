module Stage2.Tree.Definition4 where

import Data.Kind (Type)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage2.Connect (connect)
import qualified Stage2.Connect as Connect
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Term0 as Term0
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve, Stage)
import Stage2.Tree.Combinators.Implicit (Implicit)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import Stage2.Tree.Combinators.Inferred (Inferred)
import qualified Stage2.Tree.Combinators.Inferred as Inferred
import {-# SOURCE #-} Stage2.Tree.Declaration (Groupable (..))
import qualified Stage2.Tree.Definition2 as Mark
import Stage2.Tree.Definition3 (Definition3)
import Stage2.Tree.Scheme (Scheme)
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver
import qualified Stage4.Tree.Type as Simple

type Definition4 :: Locality -> Layout -> Stage -> Environment -> Type
data Definition4 locality layout stage scope where
  (:::) ::
    !(Annotation mark layout stage scope) ->
    !(Implicit (Definition3 mark layout stage) stage scope) ->
    Definition4 locality layout stage scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality Group stage scope
  (::::) ::
    !(Inferred (SchemeOver Types) stage scope) ->
    !(Implicit (Set locality stage) stage scope) ->
    Definition4 locality Group stage scope

infix 5 :::, ::::

instance Show (Definition4 locality layout stage scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 5) $
      showsPrec 6 annotation . showString " ::: " . showsPrec 6 definition
  showsPrec d (Link link id) =
    showParen (d > 10) $
      showString "Link "
        . showsPrec 11 link
        . showString " "
        . showsPrec 11 id
  showsPrec d (types :::: set) =
    showParen (d > 5) $
      showsPrec 6 types . showString " :::: " . showsPrec 6 set

instance Shift (Definition4 locality layout stage) where
  shift = shiftDefault

instance Shift.Functor (Definition4 locality layout stage) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: Shift.map category definition
    Link link id -> Link link id
    types :::: set -> Shift.map category types :::: Shift.map category set

instance FreeTermVariables (Definition4 locality layout) where
  freeTermVariables target = \case
    _ ::: Implicit.Resolve definition -> freeTermVariables target definition
    Link {} -> []
    _ :::: Implicit.Resolve set -> freeTermVariables target set

data Annotation mark layout stage scope where
  Annotated :: !(Scheme Position stage scope) -> Annotation Mark.Annotated layout stage scope
  Inferred :: Annotation Mark.Inferred Normal stage scope

instance Shift (Annotation mark layout stage) where
  shift = shiftDefault

instance Shift.Functor (Annotation mark layout stage) where
  map category = \case
    Annotated scheme -> Annotated (Shift.map category scheme)
    Inferred -> Inferred

instance Show (Annotation mark scope layout stage) where
  showsPrec d annotation = case annotation of
    Annotated scheme -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 scheme
    Inferred -> showString "Inferred"

newtype Types scope = Types (Strict.Vector (Simple.Type scope))
  deriving (Show)

instance Scope.Show Types where
  showsPrec = showsPrec

instance Shift Types where
  shift = shiftDefault

instance Shift.Functor Types where
  map category (Types types) = Types (Shift.map category <$> types)

newtype Set locality stage scope
  = Set (Strict.Vector (Element locality stage scope))
  deriving (Show)

instance Scope.Show (Set locality stage) where
  showsPrec = showsPrec

instance Shift (Set locality stage) where
  shift = shiftDefault

instance Shift.Functor (Set locality stage) where
  map category (Set set) = Set (Shift.map category <$> set)

instance FreeTermVariables (Set locality) where
  freeTermVariables target (Set set) = foldMap (freeTermVariables target) set

data Element locality stage scope = Element
  { element :: !(Definition3 Mark.Inferred Group stage (Scope.GroupTerm ':+ scope)),
    link :: !(Term.Link locality)
  }
  deriving (Show)

instance Shift (Element locality stage) where
  shift = shiftDefault

instance Shift.Functor (Element locality stage) where
  map category Element {element, link} =
    Element
      { element = Shift.map (Shift.Over category) element,
        link
      }

instance FreeTermVariables (Element locality) where
  freeTermVariables target Element {element} =
    freeTermVariables (FreeVariables.Over target) element

locality :: Definition4 locality Normal stage scope -> Definition4 locality' Normal stage scope
locality = \case
  annotation ::: declaration -> annotation ::: declaration

group ::
  (Term0.Index scope -> Term.Link locality) ->
  (Term.Link locality -> Groupable scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Definition4 locality Normal Resolve scope ->
  Definition4 locality Group Resolve scope
group _ _ _ (Annotated annotation ::: Implicit.Resolve definition) =
  Annotated annotation ::: Implicit.Resolve (connect definition)
group link index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Inferred.Inferred :::: Implicit.Resolve (Set $ Strict.Vector.fromList $ map go $ Set.toList set)
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
  (Term.Link locality -> Implicit (Set locality Check) Check scope) ->
  Definition4 locality Group Check scope ->
  Definition4 locality Normal Check scope
ungroup _ _ (Annotated annotation ::: Implicit.Check definition) =
  Annotated annotation ::: Implicit.Check (SchemeOver.map (SchemeOver.Map Connect.seperate) definition)
ungroup index lookup definition = case definition of
  Link index id -> Inferred ::: go id (lookup index)
  (_ :::: set) -> Inferred ::: go 0 set
  where
    go id (Implicit.Check SchemeOver {parameters, constraints, result = Set set}) =
      Implicit.Check $
        SchemeOver
          { parameters,
            constraints,
            result = Connect.seperate $ Shift.map (Shift.UngroupTerm original) element
          }
      where
        original id
          | Element {link} <- set Strict.Vector.! id =
              shift $ Term0.normal $ index link
        Element {element} = set Strict.Vector.! id
