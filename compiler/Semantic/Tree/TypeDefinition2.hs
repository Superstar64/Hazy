module Semantic.Tree.TypeDefinition2 where

import Data.Functor.Identity (Identity (..))
import qualified Data.Kind
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict.Vector
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Type0 as Type0
import Semantic.Layout (Layout, Normal)
import qualified Semantic.Layout as Layout
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)
import qualified Semantic.Tree.Combinators.Inferred as Combinators
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Synonym (Synonym)
import Semantic.Tree.Type (Type)
import {-# SOURCE #-} Semantic.Tree.TypeDeclaration (Groupable (..))
import Semantic.Tree.TypeDefinition (TypeDefinition)
import Semantic.Tree.TypeGroup (Element (..), Set (..), TypeGroup (..))
import Syntax.Position (Position)
import Syntax.Variable (QualifiedConstructor (..), QualifiedConstructorIdentifier (..), Qualifiers)

type TypeDefinition2 :: Locality -> Layout -> Stage -> Environment -> Data.Kind.Type
data TypeDefinition2 locality layout stage scope where
  (:::) ::
    !(Annotation layout stage scope) ->
    !(TypeDefinition stage scope) ->
    TypeDefinition2 locality layout stage scope
  Link :: !(Type.Link locality) -> !Int -> TypeDefinition2 locality Layout.Group stage scope
  Group ::
    !(TypeGroup locality Layout.Group stage scope) ->
    TypeDefinition2 locality Layout.Group stage scope
  Synonym ::
    !(Synonym stage scope) ->
    TypeDefinition2 locality layout stage scope

infix 5 :::

instance Show (TypeDefinition2 locality layout stage scope) where
  showsPrec d = \case
    annotation ::: definition ->
      showParen (d > 5) $
        showsPrec 6 annotation . showString " ::: " . showsPrec 6 definition
    Link link id ->
      showParen (d > 10) $
        showString "Link "
          . showsPrec 11 link
          . showString " "
          . showsPrec 11 id
    Group group ->
      showParen (d > 10) $
        showString "Group " . showsPrec 11 group
    Synonym synonym ->
      showParen (d > 10) $
        showString "Synonym"
          . showsPrec 11 synonym

instance Shift0.Functor (TypeDefinition2 locality layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (TypeDefinition2 locality layout stage) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: Shift.map category definition
    Link link id -> Link link id
    Group group -> Group (Shift.map category group)
    Synonym synonym -> Synonym (Shift.map category synonym)

instance FreeTypeVariables (TypeDefinition2 locality layout) where
  freeTypeVariables target = \case
    annotation ::: definition ->
      freeTypeVariables target annotation ++ freeTypeVariables target definition
    Link {} -> []
    Group group -> freeTypeVariables target group
    Synonym synonym -> freeTypeVariables target synonym

data Annotation layout stage scope where
  Annotated :: !(Type Position stage scope) -> Annotation layout stage scope
  Inferred :: Annotation Normal stage scope

instance Show (Annotation mark stage scope) where
  showsPrec d = \case
    Annotated typex -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 typex
    Inferred -> showString "Inferred"

instance Shift0.Functor (Annotation mark stage) where
  map = Shift.mapDefault

instance Shift.Functor (Annotation mark stage) where
  map category = \case
    Annotated typex -> Annotated (Shift.map category typex)
    Inferred -> Inferred

instance FreeTypeVariables (Annotation mark) where
  freeTypeVariables target = \case
    Annotated typex -> freeTypeVariables target typex
    Inferred -> []

locality :: TypeDefinition2 locality Normal stage scope -> TypeDefinition2 locality' Normal stage scope
locality = \case
  annotation ::: definition -> annotation ::: definition
  Synonym definition -> Synonym definition

group ::
  Qualifiers ->
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> Groupable scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDefinition2 locality Normal Resolve scope ->
  TypeDefinition2 locality Layout.Group Resolve scope
group _ _ _ _ (Annotated typex ::: definition) = Annotated typex ::: definition
group _ _ _ _ (Synonym definition) = Synonym definition
group qualifiers link index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group (Inferred.Inferred :::: Set (Strict.Vector.fromList $ map go $ Set.toList set))
    where
      go link = case index link of
        Groupable {element, position', name', constructorNames'} ->
          Element
            { element = Shift.map (Shift.GroupType lookup) element,
              typex = Combinators.Inferred,
              position = position',
              name = qualifiers :=. name',
              constructorNames = (qualifiers :=) <$> constructorNames',
              link
            }
      lookup index = Strict.Maybe.fromLazy $ Set.lookupIndex (link index) set
  StronglyConnected.Link {link, id} -> Link link id

ungroup ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> Set locality Check scope) ->
  TypeDefinition2 locality Layout.Group Check scope ->
  TypeDefinition2 locality Normal Check scope
ungroup index lookup definition = runIdentity $ ungroupM index (Identity . lookup) definition

ungroupM ::
  (Monad m) =>
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> m (Set locality Check scope)) ->
  TypeDefinition2 locality Layout.Group Check scope ->
  m (TypeDefinition2 locality Normal Check scope)
ungroupM _ _ (Annotated annotation ::: definition) = pure $ Annotated annotation ::: definition
ungroupM _ _ (Synonym definition) = pure $ Synonym definition
ungroupM index lookup definition = case definition of
  Link index id -> do
    set <- lookup index
    pure $ Inferred ::: go id set
  Group (_ :::: set) -> pure $ Inferred ::: go 0 set
  where
    go id (Set set) = Shift.map (Shift.UngroupType original) element
      where
        original id | Element {link} <- set Strict.Vector.! id = Type0.normal $ index link
        Element {element} = set Strict.Vector.! id
