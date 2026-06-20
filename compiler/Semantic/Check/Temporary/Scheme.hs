{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Go.Scheme as Solved (Scheme (..))
import Semantic.Check.LocalBinding (LocalBinding (Wobbly, label, wobbly))
import qualified Semantic.Check.Simple.Type as Simple.Type
import Semantic.Check.Temporary.Constraints (Constraints)
import qualified Semantic.Check.Temporary.Constraints as Constraints
import Semantic.Check.Temporary.Type (Type)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Type as Type (check, solve)
import Semantic.Check.Temporary.TypePattern (TypePattern (TypePattern))
import qualified Semantic.Check.Temporary.TypePattern as TypePattern
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type (Table (..))
import qualified Semantic.Label.Binding.Local as Label
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift (..))
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Scheme as Semantic (Scheme (..))
import qualified Semantic.Tree.TypePattern as Semantic (TypePattern (..))
import qualified Semantic.Tree.TypePattern as Solved (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Scheme s scope = Scheme
  { startPosition :: !Position,
    implicit :: !Bool,
    parameters :: !(Strict.Vector (TypePattern s scope)),
    constraints :: !(Constraints s scope),
    result :: !(Type s (Local ':+ scope))
  }

check :: Context s scope -> Semantic.Scheme Position Resolve scope -> ST s (Scheme s scope)
check context Semantic.Scheme {startPosition, implicit, parameters, constraints, result} = do
  let fresh Semantic.TypePattern {position, name} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure TypePattern {position, name, typex}
  parameters <- traverse fresh parameters
  constraints <- Constraints.check (augment parameters context) constraints
  result <- Type.check (augment parameters context) Unify.typex result
  pure $ Scheme {startPosition, implicit, parameters, constraints, result}

solve :: Context s scope -> Scheme s scope -> Unify.Solve s (Solved.Scheme Position Check scope)
solve context Scheme {startPosition, implicit, parameters = wobbly, constraints, result} = do
  parameters <- traverse TypePattern.solve wobbly
  constraints <- Constraints.solve (augment wobbly context) constraints
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
