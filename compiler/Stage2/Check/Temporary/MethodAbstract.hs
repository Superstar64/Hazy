module Stage2.Check.Temporary.MethodAbstract where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Method (Method (..))
import qualified Stage2.Tree.MethodAbstract as Solved
import qualified Stage2.Tree.MethodAbstract as Stage2
import Stage2.Check.Context (Context)
import qualified Stage2.Check.Mask as Mask
import Stage2.Check.Simple.SchemeOver (augment')
import qualified Stage2.Check.Simple.Type as Simple.Type
import Stage2.Check.Temporary.Definition (Definition)
import qualified Stage2.Check.Temporary.Definition as Definition
import qualified Stage2.Unify as Unify
import qualified Stage4.Tree.Scheme as Simple (Scheme (..), simplify)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))

data MethodAbstract s scope
  = Abstract
  | DefaultCheck !(Definition s (Local ':+ scope))

check ::
  Context s scope ->
  Position ->
  Method Check scope ->
  Stage2.MethodAbstract Group Resolve scope ->
  ST s (MethodAbstract s scope)
check _ _ _ Stage2.Abstract = pure Abstract
check context position Method {annotation} (Stage2.DefaultResolve definition)
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
