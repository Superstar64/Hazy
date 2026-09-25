module Semantic.Check.Go.Instance (check, solve, Key (..)) where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraints as Core.Constraints
import Data.Functor.Identity (Identity (..))
import Error (cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..))
import Semantic.Check.Go.Definition4 (Solve)
import Semantic.Check.Go.InstanceDefinition2 (Key (..))
import qualified Semantic.Check.Go.InstanceDefinition2 as InstanceDefinition2
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Instance (Instance (..))
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import Semantic.Tree.InstanceDefinition2 (Annotation (..), Header (..), InstanceDefinition2 (..))
import qualified Semantic.Unify as Unify
import Prelude hiding (head)

check ::
  (Monad solve) =>
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> Instance solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  Instance Identity Identity Group Resolve scope ->
  Instance solve (Topological.Formula declarations s) Group Check scope
check key solve reflection information Instance {startPosition, definition} =
  Instance
    { startPosition,
      definition =
        let reflection' declarations = case reflection declarations of
              Instance {definition} -> definition
         in InstanceDefinition2.check startPosition key solve reflection' information definition,
      prerequisites =
        Topological.Formula
          { cycle = cyclicalTypeChecking startPosition,
            run = \declarations -> do
              let Instance {definition} = reflection declarations
              case definition of
                annotation ::: _ -> do
                  Header {prerequisites} <- case annotation of
                    Standard header -> header
                    DerivedInstance header -> header
                  pure $ Solved $ Core.Constraints.simplify prerequisites
          }
    }

solve ::
  Instance (Unify.Solve s) Identity Group Check scope ->
  Unify.Solve s (Instance Identity Identity Group Check scope)
solve Semantic.Instance {startPosition, definition, prerequisites} = do
  definition <- InstanceDefinition2.solve definition
  pure Instance {startPosition, definition, prerequisites}
