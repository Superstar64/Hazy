module Stage2.Check.Temporary.Definition3 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), Local)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Definition2 (Annotated, Inferred)
import Stage2.Tree.Definition3 (Info)
import qualified Stage2.Tree.Definition3 as Solved (Definition3 (..))
import qualified Stage2.Tree.Definition3 as Stage2 (Definition3 (..))
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Definition2 (Definition2)
import qualified Stage2.Check.Temporary.Definition2 as Definition2
import qualified Stage2.Unify as Unify

data Definition3 mark s scope where
  Label :: !(Info source) -> !(Definition2 source mark s scope) -> Definition3 mark s scope

checkManual ::
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Stage2.Definition3 Annotated Group Resolve scopes ->
  ST s (Definition3 Annotated s (Local ':+ scopes))
checkManual context typex (Stage2.Label info definition) =
  Label info <$> Definition2.checkManual context typex definition

checkAuto ::
  Context s scopes ->
  Unify.Type s scopes ->
  Stage2.Definition3 Inferred Group Resolve scopes ->
  ST s (Definition3 Inferred s scopes)
checkAuto context typex (Stage2.Label info definition) =
  Label info <$> Definition2.checkAuto context typex definition

solve :: Position -> Definition3 mark s scope -> Unify.Solve s (Solved.Definition3 mark Group Check scope)
solve position (Label info definition) = do
  definition <- Definition2.solve position definition
  pure $ Solved.Label info definition
