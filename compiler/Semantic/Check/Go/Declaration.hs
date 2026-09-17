module Semantic.Check.Go.Declaration where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Forall as Forall
import Core.Tree.Type (TypeF)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Definition4 as Definition4
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Layout as Layout
import Semantic.Stage (Check)
import qualified Semantic.Stage as Stage
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Declaration (Declaration (..))
import Semantic.Tree.Definition4 (Annotation (..), Definition4 (..))
import Semantic.Tree.Group (Group ((::::)), Types (..))
import Semantic.Unify (ForallOver)
import qualified Semantic.Unify as Unify

check ::
  (Functor solve) =>
  Term.Link locality ->
  Definition4.Solve solve logical s scope ->
  ( declarations (ST s) ->
    Term.Link locality ->
    Declaration solve' logical locality (ST s) Layout.Group Stage.Check scope
  ) ->
  (declarations (ST s) -> Context s scope) ->
  Declaration Identity Void locality Identity Layout.Group Stage.Resolve scope ->
  Declaration solve logical locality (Topological.Formula declarations s) Layout.Group Stage.Check scope
check link solve reflection information Declaration {position, name, definition} =
  Declaration
    { position,
      name,
      definition =
        let reflection' declarations = case reflection declarations link of
              Declaration {definition} -> definition
         in Definition4.check position solve reflection' information definition,
      typex =
        Topological.Formula
          { cycle = cyclicalTypeChecking position,
            run = \declarations ->
              let Declaration {definition} = reflection declarations link
                  inferred ::
                    Int ->
                    Group solve' logical locality Layout.Group Stage.Check scope ->
                    Inferred (ForallOver TypeF logical) Stage.Check scope
                  inferred index (Solved types :::: _) = Solved $ Forall.map map types
                    where
                      map = Forall.Map $ \(Types types) -> types Strict.Vector.! index
               in case definition of
                    Annotated annotation ::: _ -> do
                      scheme <- annotation
                      pure $ Solved (logicalType $ Forall.simplify scheme)
                    Link link id -> do
                      let Declaration {definition} = reflection declarations link
                      case definition of
                        Group group -> inferred id <$> group
                        _ -> error "bad self declaration"
                    Group group -> inferred 0 <$> group
          }
    }

solve ::
  Declaration (Unify.Solve s) (Unify.Logical s scope) locality Identity Layout.Group Check scope ->
  Unify.Solve s (Declaration Identity Void locality Identity Layout.Group Check scope)
solve Declaration {position, name, definition, typex = Identity (Solved typex)} = do
  definition <- Definition4.solveDefinition position definition
  typex <- Unify.solve position typex
  pure
    Declaration
      { position,
        name,
        definition,
        typex =
          Identity (Solved typex)
      }
