module Semantic.Check.Go.MethodAbstract where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Forall as Simple (ForallOver (..), simplify)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Scheme (augmentForall)
import qualified Semantic.Check.Temporary.Definition as Definition
import Semantic.Layout (Group)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Method (Method (..))
import Semantic.Tree.MethodAbstract (MethodAbstract (..))
import qualified Semantic.Tree.MethodAbstract as Semantic
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

check ::
  Context s scope ->
  Position ->
  Method Check scope ->
  MethodAbstract Group Resolve scope ->
  ST s (Unify.Solve s (MethodAbstract Group Check scope))
check _ _ _ Semantic.Abstract = pure $ do
  pure Abstract
check context position Method {annotation} (Semantic.DefaultResolve definition)
  | scheme@Simple.ForallOver {result} <- Simple.simplify annotation = do
      context <- augmentForall position scheme Mask.Runtime context
      definition <- Definition.check context (logicalType result) (shift definition)
      pure $ do
        definition <- Definition.solve definition
        pure $ DefaultCheck definition
