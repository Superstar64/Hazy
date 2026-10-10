module Semantic.Check.Temporary.Constraint where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import Data.Foldable (toList)
import Data.List.Reverse (List (Nil, (:>)))
import qualified Data.List.Reverse as Reverse
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureEqualityConstraints)
import qualified Semantic.Check.Binding.Local as LocalBinding
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type (check, solve)
import Semantic.Index.Local (Index (Local))
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraint as Semantic
import qualified Semantic.Tree.Constraint as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (head)

data Constraint s scope = Constraint
  { startPosition :: !Position,
    classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type s (Local ':+ scope)))
  }

check ::
  Context s (Local ':+ scope) ->
  Semantic.Constraint Position Resolve scope ->
  ST s (Constraint s scope)
check
  context@Context {localEnvironment}
  Semantic.Constraint
    { startPosition,
      classx,
      head,
      arguments
    } = do
    universe <- Unify.fresh Core.universe
    target <- Unify.fresh (Core.typeWith universe)
    real <- Context.lookupKind startPosition context (shift classx)

    Unify.unify context startPosition (target -#> Core.constraint) real

    let check context kind (arguments :> argument) = do
          universe <- Unify.fresh Core.universe
          parameterType <- Unify.fresh (Core.typeWith universe)
          arguments <- check context (parameterType -#> kind) arguments
          argument <- Type.check context parameterType argument
          pure (arguments :> argument)
        check context kind Nil = case localEnvironment Table.Local.! Local head of
          LocalBinding.Wobbly {wobbly} -> do
            Unify.unify context startPosition kind wobbly
            pure Nil
          LocalBinding.Rigid {rigid} -> do
            Unify.unify context startPosition kind (logicalType rigid)
            pure Nil

    arguments <- Strict.Vector.fromList . toList <$> check context target (Reverse.fromList $ toList arguments)

    pure $ Constraint {startPosition, classx, head, arguments}
check _ Semantic.Equality {startPosition} = unsupportedFeatureEqualityConstraints startPosition

solve :: Context s (Local ':+ scope) -> Constraint s scope -> Unify.Solve s (Solved.Constraint Position Check scope)
solve context Constraint {startPosition, classx, head, arguments} = do
  arguments <- traverse (Type.solve context) arguments
  pure
    Solved.Constraint
      { startPosition,
        classx,
        head,
        arguments
      }
