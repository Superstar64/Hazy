module Stage2.Tree.Definition3 where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graph.StronglyConnected as StronglyConnected
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.Group.Index.Link.Term as Group.Term
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Annotated, Definition2, Inferred, Mark, Source)
import qualified Stage2.Tree.Definition2 as Definition2

type Definition3 :: Locality -> Source -> Mark -> Layout -> Environment -> Type
data Definition3 locality source mark layout scope where
  Manual :: !(Definition2 source Annotated scope) -> Definition3 locality source Annotated layout scope
  Auto :: !(Definition2 source Inferred scope) -> Definition3 locality source Inferred Normal scope
  Link :: !(Group.Term.Link locality) -> Definition3 locality source Inferred Group scope
  Group ::
    !(Map (Group.Term.Link locality) (Definition2.Auto scope)) ->
    Definition3 locality source Inferred Group scope

instance Show (Definition3 locality source mark layout scope) where
  showsPrec d (Manual definition) =
    showParen (d > 10) $
      showString "Manual " . showsPrec 11 definition
  showsPrec d (Auto definition) =
    showParen (d > 10) $
      showString "Auto " . showsPrec 11 definition
  showsPrec d (Link link) = showParen (d > 10) $ showString "Link " . showsPrec 11 link
  showsPrec d (Group set) = showParen (d > 10) $ showString "Group " . showsPrec 11 set

instance Shift (Definition3 locality source layout mark) where
  shift = shiftDefault

instance Shift.Functor (Definition3 locality source layout mark) where
  map category = \case
    Manual definition -> Manual (Shift.map category definition)
    Auto definition -> Auto (Shift.map category definition)
    Link link -> Link link
    Group set -> Group (Shift.map category <$> set)

instance FreeTermVariables (Definition3 locality source layout mark) where
  freeTermVariables target = \case
    Manual definition -> freeTermVariables target definition
    Auto definition -> freeTermVariables target definition
    Link {} -> []
    Group set -> foldMap (freeTermVariables target) set

locality :: Definition3 locality source mark Normal scope -> Definition3 locality' source mark Normal scope
locality = \case
  Manual declaration -> Manual declaration
  Auto declaration -> Auto declaration

group ::
  (Group.Term.Link locality -> Definition2.Auto scope) ->
  StronglyConnected.Component (Group.Term.Link locality) ->
  Definition3 locality source mark Normal scope ->
  Definition3 locality source mark Group scope
group _ _ (Manual definition) = Manual definition
group index group Auto {} = case group of
  StronglyConnected.Group {set} ->
    Group $ Map.fromSet index set
  StronglyConnected.Link {link} -> Link link
