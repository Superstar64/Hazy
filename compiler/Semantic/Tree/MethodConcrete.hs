module Semantic.Tree.MethodConcrete where

import qualified Core.Tree.TypeLambda as Simple (TypeLambda)
import Semantic.Connect (Connect (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Definition (Definition)

data Origin
  = Manual
  | Auto

type Manual = 'Manual

type Auto = 'Auto

data MethodConcrete origin layout stage scope where
  Definition ::
    !(Implicit Definition layout stage (Local ':+ scope)) ->
    MethodConcrete Manual layout stage scope
  Generated ::
    !(Inferred (Simple.TypeLambda) stage (Local ':+ scope)) ->
    MethodConcrete origin layout stage scope

instance Show (MethodConcrete origin layout stage scope) where
  showsPrec d = \case
    Definition definition -> showParen (d > 10) $ showString "Definition " . showsPrec 11 definition
    Generated automatic -> showParen (d > 10) $ showString "Generated " . showsPrec 11 automatic

instance Shift0.Functor (MethodConcrete origin layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (MethodConcrete origin layout stage) where
  map category = \case
    Definition definition -> Definition $ Shift.map (Shift.Over category) definition
    Generated automatic -> Generated $ Shift.map (Shift.Over category) automatic

instance Connect (MethodConcrete origin) where
  connect = \case
    Definition definition -> Definition $ connect definition
    Generated {} -> Generated Inferred

  seperate = \case
    Definition definition -> Definition $ seperate definition
    Generated automatic -> Generated automatic
