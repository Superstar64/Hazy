module Semantic.Tree.MethodConcrete where

import qualified Core.Tree.TypeLambda as Simple (TypeLambda)
import Semantic.Connect (Connect (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Definition (Definition)

data MethodConcrete layout stage scope
  = Definition !(Implicit Definition layout stage (Local ':+ scope))
  | Generated !(Inferred (Simple.TypeLambda) stage (Local ':+ scope))
  deriving (Show)

instance Shift0.Functor (MethodConcrete layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (MethodConcrete layout stage) where
  map category = \case
    Definition definition -> Definition $ Shift.map (Shift.Over category) definition
    Generated automatic -> Generated $ Shift.map (Shift.Over category) automatic

instance Connect MethodConcrete where
  connect = \case
    Definition definition -> Definition $ connect definition
    Generated {} -> Generated Inferred

  seperate = \case
    Definition definition -> Definition $ seperate definition
    Generated automatic -> Generated automatic
