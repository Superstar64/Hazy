module Stage2.Tree.TypeDefinition2 where

import qualified Data.Kind
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Type0 as Type0
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve, Stage)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition)

type TypeDefinition2 :: Locality -> Layout -> Stage -> Environment -> Data.Kind.Type
data TypeDefinition2 locality layout stage scope where
  (:::) ::
    !(Annotation layout stage scope) ->
    !(TypeDefinition stage scope) ->
    TypeDefinition2 locality layout stage scope
  Link :: !(Type.Link locality) -> !Int -> TypeDefinition2 locality Group stage scope
  Group ::
    !(Strict.Vector (Element locality stage (Scope.GroupType ':+ scope))) ->
    TypeDefinition2 locality Group stage scope

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
    Group set -> showParen (d > 10) $ showString "Group " . showsPrec 11 set

instance Shift (TypeDefinition2 locality layout stage) where
  shift = shiftDefault

instance Shift.Functor (TypeDefinition2 locality layout stage) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: Shift.map category definition
    Link link id -> Link link id
    Group set -> Group $ Shift.map (Shift.Over category) <$> set

instance FreeTypeVariables (TypeDefinition2 locality layout) where
  freeTypeVariables target = \case
    annotation ::: definition ->
      freeTypeVariables target annotation ++ freeTypeVariables target definition
    Link {} -> []
    Group set -> foldMap (freeTypeVariables (FreeVariables.Over target)) set

data Annotation layout stage scope where
  Annotated :: !(Type Position stage scope) -> Annotation layout stage scope
  Inferred :: Annotation Normal stage scope

instance Show (Annotation mark stage scope) where
  showsPrec d = \case
    Annotated typex -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 typex
    Inferred -> showString "Inferred"

instance Shift (Annotation mark stage) where
  shift = shiftDefault

instance Shift.Functor (Annotation mark stage) where
  map category = \case
    Annotated typex -> Annotated (Shift.map category typex)
    Inferred -> Inferred

instance FreeTypeVariables (Annotation mark) where
  freeTypeVariables target = \case
    Annotated typex -> freeTypeVariables target typex
    Inferred -> []

data Element locality stage scope = Element
  { element :: !(TypeDefinition stage scope),
    link :: !(Type.Link locality)
  }
  deriving (Show)

instance Shift (Element locality stage) where
  shift = shiftDefault

instance Shift.Functor (Element locality stage) where
  map category Element {element, link} =
    Element
      { element = Shift.map category element,
        link
      }

instance FreeTypeVariables (Element locality) where
  freeTypeVariables target Element {element} =
    freeTypeVariables target element

locality :: TypeDefinition2 locality Normal stage scope -> TypeDefinition2 locality' Normal stage scope
locality = \case
  annotation ::: definition -> annotation ::: definition

group ::
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> TypeDefinition Resolve scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDefinition2 locality Normal Resolve scope ->
  TypeDefinition2 locality Group Resolve scope
group _ _ _ (Annotated typex ::: definition) = Annotated typex ::: definition
group link index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group $ Strict.Vector.fromList $ map go $ Set.toList set
    where
      go link =
        Element
          { element = Shift.map (Shift.GroupType lookup) $ index link,
            link
          }
      lookup index = Strict.Maybe.fromLazy $ Set.lookupIndex (link index) set
  StronglyConnected.Link {link, id} -> Link link id

ungroup ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> Strict.Vector (Element locality Check (Scope.GroupType ':+ scope))) ->
  TypeDefinition2 locality Group Check scope ->
  TypeDefinition2 locality Normal Check scope
ungroup _ _ (Annotated annotation ::: definition) = Annotated annotation ::: definition
ungroup index lookup definition = case definition of
  Link index id -> Inferred ::: go id (lookup index)
  (Group set) -> Inferred ::: go 0 set
  where
    go id set = Shift.map (Shift.UngroupType original) element
      where
        original id | Element {link} <- set Strict.Vector.! id = index link
        Element {element} = set Strict.Vector.! id
