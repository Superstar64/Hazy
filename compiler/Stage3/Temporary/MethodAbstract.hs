module Stage3.Temporary.MethodAbstract where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Method (Method (..))
import qualified Stage2.Tree.MethodAbstract as Solved
import qualified Stage2.Tree.MethodAbstract as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Mask as Mask
import Stage3.Simple.SchemeOver (augment')
import qualified Stage3.Simple.Type as Simple.Type
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Scheme as Simple (Scheme (..), simplify)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))

data MethodAbstract s scope
  = Abstract
  | DefaultCheck !(Definition s (Local ':+ scope))

instance Unify.Zonk MethodAbstract where
  zonk zonker = \case
    Abstract -> pure Abstract
    DefaultCheck definition ->
      DefaultCheck <$> Unify.zonk zonker definition

check ::
  Context s scope ->
  Position ->
  Method Check scope ->
  Stage2.MethodAbstract Normal Resolve scope ->
  ST s (MethodAbstract s scope)
check _ _ _ Stage2.Abstract = pure Abstract
check context position Method {annotation} (Stage2.DefaultResolve definition)
  | Simple.Scheme scheme@Simple.SchemeOver {result} <- Simple.simplify annotation = do
      context <- augment' position scheme Mask.Runtime context
      definition <- Definition.check context (Simple.Type.lift result) (shift definition)
      pure $ DefaultCheck definition

solve :: MethodAbstract s scope -> ST s (Solved.MethodAbstract Normal Check scope)
solve = \case
  Abstract -> pure Solved.Abstract
  DefaultCheck definition -> do
    definition <- Definition.solve definition
    pure $ Solved.DefaultCheck definition
