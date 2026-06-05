module Semantic.Check.Temporary.MethodAbstract where

import Control.Monad.ST (ST)
import qualified Core.Tree.Scheme as Simple (Scheme (..), simplify)
import qualified Core.Tree.SchemeOver as Simple (SchemeOver (..))
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Simple.SchemeOver (augment')
import qualified Semantic.Check.Simple.Type as Simple.Type
import Semantic.Check.Temporary.Definition (Definition)
import qualified Semantic.Check.Temporary.Definition as Definition
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Method (Method (..))
import qualified Semantic.Tree.MethodAbstract as Semantic
import qualified Semantic.Tree.MethodAbstract as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data MethodAbstract s scope
  = Abstract
  | DefaultCheck !(Definition s (Local ':+ scope))

check ::
  Context s scope ->
  Position ->
  Method Check scope ->
  Semantic.MethodAbstract Group Resolve scope ->
  ST s (MethodAbstract s scope)
check _ _ _ Semantic.Abstract = pure Abstract
check context position Method {annotation} (Semantic.DefaultResolve definition)
  | Simple.Scheme scheme@Simple.SchemeOver {result} <- Simple.simplify annotation = do
      context <- augment' position scheme Mask.Runtime context
      definition <- Definition.check context (Simple.Type.lift result) (shift definition)
      pure $ DefaultCheck definition

solve :: MethodAbstract s scope -> Unify.Solve s (Solved.MethodAbstract Group Check scope)
solve = \case
  Abstract -> pure Solved.Abstract
  DefaultCheck definition -> do
    definition <- Definition.solve definition
    pure $ Solved.DefaultCheck definition
