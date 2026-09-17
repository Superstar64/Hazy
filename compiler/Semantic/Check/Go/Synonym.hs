module Semantic.Check.Go.Synonym where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Core.Type
import Core.Type.Functor (shiftLogical)
import qualified Data.Strict.Maybe as Strict
import Error (Position)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Temporary.Scheme as Unsolved (augment)
import qualified Semantic.Check.Temporary.Type as Type
import qualified Semantic.Check.Temporary.TypePattern as Unsolved (TypePattern (..), solve)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Synonym (Synonym (..), SynonymBody (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify

check :: Context s scope -> Position -> Synonym Resolve scope -> ST s (Synonym Check scope)
check context position (annotation ::: SynonymBody {parameters, synonym}) = do
  universe <- Unify.fresh Core.universe
  kind <- Unify.fresh (Core.typeWith universe)
  annotation <- case annotation of
    Strict.Nothing -> pure Strict.Nothing
    Strict.Just annotation -> do
      kind' <- Type.check context (Core.typeWith universe) annotation
      kind' <- Unify.runSolve $ Type.solve context kind'
      kindx <- pure $ logicalType $ Core.Type.simplify kind'
      Unify.unify context position kind kindx
      pure $ Strict.Just kind'
  let fresh TypePattern {name, position} = do
        level <- Unify.fresh Core.universe
        typex <- Unify.fresh (Core.typeWith level)
        pure
          Unsolved.TypePattern
            { name,
              typex,
              position
            }
  parameters <- traverse fresh parameters
  target <- Unify.fresh (Core.typeWith universe)
  kind' <- pure $ foldr ((-#>) . Unsolved.typex) target parameters
  Unify.unify context position kind kind'
  context <- pure $ Unsolved.augment parameters context
  synonym <- Type.check context (shiftLogical target) synonym
  parameters <- Unify.runSolve $ traverse Unsolved.solve parameters
  synonym <- Unify.runSolve $ Type.solve context synonym
  kind <- Unify.runSolve $ Unify.solve position kind
  pure $ annotation ::: SynonymBody {kind = Solved kind, parameters, synonym}
