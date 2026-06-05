module Semantic.Check.Temporary.Definition3 where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Definition2 (Definition2)
import qualified Semantic.Check.Temporary.Definition2 as Definition2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Definition2 (Annotated, Inferred)
import Semantic.Tree.Definition3 (Info)
import qualified Semantic.Tree.Definition3 as Semantic (Definition3 (..))
import qualified Semantic.Tree.Definition3 as Solved (Definition3 (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Definition3 mark s scope where
  Label :: !(Info source) -> !(Definition2 source mark s scope) -> Definition3 mark s scope

checkManual ::
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Semantic.Definition3 Annotated Group Resolve scopes ->
  ST s (Definition3 Annotated s (Local ':+ scopes))
checkManual context typex (Semantic.Label info definition) =
  Label info <$> Definition2.checkManual context typex definition

checkAuto ::
  Context s scopes ->
  Unify.Type s scopes ->
  Semantic.Definition3 Inferred Group Resolve scopes ->
  ST s (Definition3 Inferred s scopes)
checkAuto context typex (Semantic.Label info definition) =
  Label info <$> Definition2.checkAuto context typex definition

solve :: Position -> Definition3 mark s scope -> Unify.Solve s (Solved.Definition3 mark Group Check scope)
solve position (Label info definition) = do
  definition <- Definition2.solve position definition
  pure $ Solved.Label info definition
