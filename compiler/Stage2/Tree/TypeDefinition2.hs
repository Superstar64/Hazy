module Stage2.Tree.TypeDefinition2 where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.Index.Link.Type as Type
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.TypeDefinition (TypeDefinition)

data Mark
  = Annotated
  | Inferred

type Annotated = 'Annotated

type Inferred = 'Inferred

type TypeDefinition2 :: Locality -> Mark -> Layout -> Environment -> Type
data TypeDefinition2 locality mark layout scope where
  Manual :: !(TypeDefinition scope) -> TypeDefinition2 locality Annotated layout scope
  Auto :: !(TypeDefinition scope) -> TypeDefinition2 locality Inferred Normal scope
  Link :: !(Type.Link locality) -> TypeDefinition2 locality Inferred Group scope
  Group ::
    !(Type.Link locality) ->
    !(Map (Type.Link locality) (TypeDefinition scope)) ->
    TypeDefinition2 locality Inferred Group scope

instance Show (TypeDefinition2 locality mark layout scope) where
  showsPrec d = \case
    Manual definition -> showParen (d > 10) $ showString "Manual " . showsPrec 11 definition
    Auto definition -> showParen (d > 10) $ showString "Auto " . showsPrec 11 definition
    Link link -> showParen (d > 10) $ showString "Link " . showsPrec 11 link
    Group link set ->
      showParen (d > 10) $
        showString "Group "
          . showsPrec 11 link
          . showString " "
          . showsPrec 11 set

instance Shift (TypeDefinition2 locality mark layout) where
  shift = shiftDefault

instance Shift.Functor (TypeDefinition2 locality mark layout) where
  map category = \case
    Manual definition -> Manual (Shift.map category definition)
    Auto definition -> Auto (Shift.map category definition)
    Link link -> Link link
    Group link set -> Group link $ Shift.map category <$> set

instance FreeTypeVariables (TypeDefinition2 locality mark layout) where
  freeTypeVariables target = \case
    Manual definition -> freeTypeVariables target definition
    Auto definition -> freeTypeVariables target definition
    Link {} -> []
    Group _ set -> foldMap (freeTypeVariables target) set

locality :: TypeDefinition2 locality mark Normal scope -> TypeDefinition2 locality' mark Normal scope
locality = \case
  Manual definition -> Manual definition
  Auto definition -> Auto definition

group ::
  (Type.Link locality -> TypeDefinition scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  TypeDefinition2 locality mark Normal scope ->
  TypeDefinition2 locality mark Group scope
group _ _ (Manual definition) = Manual definition
group index group Auto {} = case group of
  StronglyConnected.Group {link, set} ->
    Group link $ Map.fromSet index set
  StronglyConnected.Link {link} -> Link link
