{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type (Table (..))
import qualified Stage2.Label.Binding.Local as Label
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift (..))
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (..))
import qualified Stage2.Tree.TypePattern as Solved (TypePattern (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.LocalBinding (LocalBinding (Wobbly, label, wobbly))
import qualified Stage3.Simple.Type as Simple.Type
import Stage3.Temporary.Constraint (Constraint)
import qualified Stage3.Temporary.Constraint as Constraint
import Stage3.Temporary.Type (Type)
import {-# SOURCE #-} qualified Stage3.Temporary.Type as Type (check, solve)
import Stage3.Temporary.TypePattern (TypePattern (TypePattern))
import qualified Stage3.Temporary.TypePattern as TypePattern
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Unify as Unify

data Scheme s scope = Scheme
  { startPosition :: !Position,
    implicit :: !Bool,
    parameters :: !(Strict.Vector (TypePattern s scope)),
    constraints :: !(Strict.Vector (Constraint s scope)),
    result :: !(Type s (Local ':+ scope))
  }

check :: Context s scope -> Stage2.Scheme Position Resolve scope -> ST s (Scheme s scope)
check context (Stage2.Scheme {startPosition, implicit, parameters, constraints, result}) = do
  let fresh Stage2.TypePattern {position, name} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure TypePattern {position, name, typex}
  parameters <- traverse fresh parameters
  constraints <- traverse (Constraint.check (augment parameters context)) constraints
  result <- Type.check (augment parameters context) Unify.typex result
  pure $ Scheme {startPosition, implicit, parameters, constraints, result}

solve :: Context s scope -> Scheme s scope -> Unify.Solve s (Solved.Scheme Position Check scope)
solve context Scheme {startPosition, implicit, parameters = wobbly, constraints, result} = do
  parameters <- traverse TypePattern.solve wobbly
  constraints <- traverse (Constraint.solve (augment wobbly context)) constraints
  result <- Type.solve (augment wobbly context) result
  pure $ Solved.Scheme {startPosition, implicit, parameters, constraints, result}

augment :: Strict.Vector (TypePattern s scope) -> Context s scope -> Context s (Local ':+ scope)
augment scheme Context {termEnvironment, localEnvironment, typeEnvironment}
  | scheme <- Strict.Vector.toLazy scheme =
      Context
        { termEnvironment = Term.Local termEnvironment,
          localEnvironment = Local.Local (fmap wobbly scheme) localEnvironment,
          typeEnvironment = Type.Local typeEnvironment
        }
  where
    wobbly
      TypePattern {name, typex = wobbly} =
        Wobbly
          { label = Label.LocalBinding {name},
            wobbly = shift wobbly
          }

augmentSolve ::
  forall s scope.
  Strict.Vector (Solved.TypePattern Position Check scope) ->
  Context s scope ->
  Context s (Local ':+ scope)
augmentSolve scheme Context {termEnvironment, localEnvironment, typeEnvironment}
  | scheme <- Strict.Vector.toLazy scheme =
      Context
        { termEnvironment = Term.Local termEnvironment,
          localEnvironment = Local.Local (fmap wobbly scheme) localEnvironment,
          typeEnvironment = Type.Local typeEnvironment
        }
  where
    wobbly :: Solved.TypePattern position Check scope -> LocalBinding s (scope' ':+ scope)
    wobbly
      Solved.TypePattern {name, typex = Solved wobbly} =
        Wobbly
          { label = Label.LocalBinding {name},
            wobbly = Simple.Type.lift $ shift wobbly
          }
