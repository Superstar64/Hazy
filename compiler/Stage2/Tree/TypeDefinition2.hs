module Stage2.Tree.TypeDefinition2 where

import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.Index.Link.Type as Type
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition)

type TypeDefinition2 :: Locality -> Layout -> Environment -> Data.Kind.Type
data TypeDefinition2 locality layout scope where
  (:::) :: !(Annotation layout scope) -> !(TypeDefinition scope) -> TypeDefinition2 locality layout scope
  Link :: !(Type.Link locality) -> TypeDefinition2 locality Group scope
  Group ::
    !(Map (Type.Link locality) (TypeDefinition scope)) ->
    TypeDefinition2 locality Group scope

infix 5 :::

instance Show (TypeDefinition2 locality layout scope) where
  showsPrec d = \case
    annotation ::: definition ->
      showParen (d > 5) $
        showsPrec 6 annotation . showString " ::: " . showsPrec 6 definition
    Link link -> showParen (d > 10) $ showString "Link " . showsPrec 11 link
    Group set -> showParen (d > 10) $ showString "Group " . showsPrec 11 set

instance Shift (TypeDefinition2 locality layout) where
  shift = shiftDefault

instance Shift.Functor (TypeDefinition2 locality layout) where
  map category = \case
    annotation ::: definition -> Shift.map category annotation ::: Shift.map category definition
    Link link -> Link link
    Group set -> Group $ Shift.map category <$> set

instance FreeTypeVariables (TypeDefinition2 locality layout) where
  freeTypeVariables target = \case
    annotation ::: definition ->
      freeTypeVariables target annotation ++ freeTypeVariables target definition
    Link {} -> []
    Group set -> foldMap (freeTypeVariables target) set

data Annotation layout scope where
  Annotated :: !(Type Position scope) -> Annotation layout scope
  Inferred :: Annotation Normal scope

instance Show (Annotation mark scope) where
  showsPrec d = \case
    Annotated typex -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 typex
    Inferred -> showString "Inferred"

instance Shift (Annotation mark) where
  shift = shiftDefault

instance Shift.Functor (Annotation mark) where
  map category = \case
    Annotated typex -> Annotated (Shift.map category typex)
    Inferred -> Inferred

instance FreeTypeVariables (Annotation mark) where
  freeTypeVariables target = \case
    Annotated typex -> freeTypeVariables target typex
    Inferred -> []

locality :: TypeDefinition2 locality Normal scope -> TypeDefinition2 locality' Normal scope
locality = \case
  annotation ::: definition -> annotation ::: definition

group ::
  (Type.Link locality -> TypeDefinition scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDefinition2 locality Normal scope ->
  TypeDefinition2 locality Group scope
group _ _ (Annotated typex ::: definition) = Annotated typex ::: definition
group index group (Inferred ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group $ Map.fromSet index set
  StronglyConnected.Link {link} -> Link link
