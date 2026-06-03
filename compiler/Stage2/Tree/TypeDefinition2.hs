module Stage2.Tree.TypeDefinition2 where

import Data.Functor.Identity (Identity (..))
import qualified Data.Kind
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict.Maybe
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable (QualifiedConstructor (..), QualifiedConstructorIdentifier (..), Qualifiers)
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve, Stage)
import qualified Stage2.Tree.Combinators.Inferred as Combinators
import Stage2.Tree.Type (Type)
import {-# SOURCE #-} Stage2.Tree.TypeDeclaration (Groupable (..))
import Stage2.Tree.TypeDefinition (Constructive, Substitutive, TypeDefinition)
import qualified Stage4.Tree.Type as Simple

type TypeDefinition2 :: Locality -> Layout -> Stage -> Environment -> Data.Kind.Type
data TypeDefinition2 locality layout stage scope where
  (:::) ::
    !(Annotation equality layout stage scope) ->
    !(TypeDefinition equality stage scope) ->
    TypeDefinition2 locality layout stage scope
  Link :: !(Type.Link locality) -> !Int -> TypeDefinition2 locality Group stage scope
  Group ::
    !(Set locality stage scope) ->
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
    Group set -> Group $ Shift.map category set

instance FreeTypeVariables (TypeDefinition2 locality layout) where
  freeTypeVariables target = \case
    annotation ::: definition ->
      freeTypeVariables target annotation ++ freeTypeVariables target definition
    Link {} -> []
    Group set -> freeTypeVariables target set

data Annotation equality layout stage scope where
  Annotated :: !(Type Position stage scope) -> Annotation equality layout stage scope
  InferredCyclic :: Annotation Constructive Normal stage scope
  InferredAcyclic :: Annotation Substitutive layout stage scope

instance Show (Annotation equality mark stage scope) where
  showsPrec d = \case
    Annotated typex -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 typex
    InferredCyclic -> showString "InferredCyclic"
    InferredAcyclic -> showString "InferredAcyclic"

instance Shift (Annotation equality mark stage) where
  shift = shiftDefault

instance Shift.Functor (Annotation equality mark stage) where
  map category = \case
    Annotated typex -> Annotated (Shift.map category typex)
    InferredCyclic -> InferredCyclic
    InferredAcyclic -> InferredAcyclic

instance FreeTypeVariables (Annotation equality mark) where
  freeTypeVariables target = \case
    Annotated typex -> freeTypeVariables target typex
    InferredCyclic -> []
    InferredAcyclic -> []

newtype Set locality stage scope
  = Set (Strict.Vector (Element locality stage scope))
  deriving (Show)

instance Shift (Set locality stage) where
  shift = shiftDefault

instance Shift.Functor (Set locality stage) where
  map category (Set set) = Set (Shift.map category <$> set)

instance FreeTypeVariables (Set locality) where
  freeTypeVariables target (Set set) = foldMap (freeTypeVariables target) set

data Element locality stage scope = Element
  { element :: !(TypeDefinition Constructive stage (Scope.GroupType ':+ scope)),
    typex :: !(Combinators.Inferred Simple.Type stage scope),
    position :: !Position,
    name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor),
    link :: !(Type.Link locality)
  }
  deriving (Show)

instance Shift (Element locality stage) where
  shift = shiftDefault

instance Shift.Functor (Element locality stage) where
  map category Element {element, typex, position, name, constructorNames, link} =
    Element
      { element = Shift.map (Shift.Over category) element,
        typex = Shift.map category typex,
        position,
        name,
        constructorNames,
        link
      }

instance FreeTypeVariables (Element locality) where
  freeTypeVariables target Element {element} =
    freeTypeVariables (FreeVariables.Over target) element

locality :: TypeDefinition2 locality Normal stage scope -> TypeDefinition2 locality' Normal stage scope
locality = \case
  annotation ::: definition -> annotation ::: definition

label :: Element locality stage scope1 -> Label.TypeBinding scope2
label Element {name, constructorNames} =
  Label.TypeBinding
    { name,
      constructorNames
    }

group ::
  Qualifiers ->
  (Type0.Index scope -> Type.Link locality) ->
  (Type.Link locality -> Groupable scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDefinition2 locality Normal Resolve scope ->
  TypeDefinition2 locality Group Resolve scope
group _ _ _ _ (Annotated typex ::: definition) = Annotated typex ::: definition
group _ _ _ _ (InferredAcyclic ::: definition) = InferredAcyclic ::: definition
group qualifiers link index group (InferredCyclic ::: _) = case group of
  StronglyConnected.Group {set} ->
    Group $ Set $ Strict.Vector.fromList $ map go $ Set.toList set
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
  TypeDefinition2 locality Group Check scope ->
  TypeDefinition2 locality Normal Check scope
ungroup index lookup definition = runIdentity $ ungroupM index (Identity . lookup) definition

ungroupM ::
  (Monad m) =>
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> m (Set locality Check scope)) ->
  TypeDefinition2 locality Group Check scope ->
  m (TypeDefinition2 locality Normal Check scope)
ungroupM _ _ (Annotated annotation ::: definition) = pure $ Annotated annotation ::: definition
ungroupM _ _ (InferredAcyclic ::: definition) = pure $ InferredAcyclic ::: definition
ungroupM index lookup definition = case definition of
  Link index id -> do
    set <- lookup index
    pure $ InferredCyclic ::: go id set
  (Group set) -> pure $ InferredCyclic ::: go 0 set
  where
    go id (Set set) = Shift.map (Shift.UngroupType original) element
      where
        original id | Element {link} <- set Strict.Vector.! id = Type0.normal $ index link
        Element {element} = set Strict.Vector.! id
