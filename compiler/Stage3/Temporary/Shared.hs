module Stage3.Temporary.Shared where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Definition2 as Stage2.Definition2
import qualified Stage2.Tree.Definition3 as Stage2.Definition3
import qualified Stage2.Tree.Shared as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import Stage3.Temporary.RightHandSide2 (RightHandSide2 (..))
import qualified Stage3.Temporary.RightHandSide2 as RightHandSide2
import qualified Stage3.Tree.Shared as Solved
import qualified Stage3.Unify as Unify

data Shared s scope = Shared
  { equalPosition :: !Position,
    body :: !(Unify.SchemeOver RightHandSide2 s scope)
  }

instance Unify.Zonk Shared where
  zonk zonker Shared {equalPosition, body} = do
    body <- Unify.zonk zonker body
    pure Shared {equalPosition, body}

check ::
  Context s scopes ->
  Maybe (Unify.Type s scopes) ->
  Stage2.Shared locality Normal scopes ->
  ST s (Shared s scopes)
check
  context
  annotation
  Stage2.Shared {definition = Stage2.Definition3.Auto (Stage2.Definition2.Shared definition), equalPosition} =
    case annotation of
      Nothing -> do
        body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
          typex <- Unify.fresh Unify.typex
          definition <- RightHandSide.check context typex (shift definition)
          pure $ RightHandSide2 {definition, typex}
        pure $ Shared {body, equalPosition}
      Just typex -> do
        body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
          definition <- RightHandSide.check context (shift typex) (shift definition)
          pure $ RightHandSide2 {definition, typex = shift typex}
        pure $ Shared {body, equalPosition}

solve :: Shared s scope -> ST s (Solved.Shared scope)
solve Shared {body, equalPosition} = do
  body <-
    Unify.solveSchemeOver
      (Unify.Solve RightHandSide2.solve)
      equalPosition
      body
  pure Solved.Shared {body, equalPosition}
