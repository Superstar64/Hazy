module Stage3.Temporary.Shared where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Shared as Stage2
import Stage3.Check.Context (Context)
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Tree.Shared as Solved
import qualified Stage3.Unify as Unify

data Shared s scope = Shared
  { equalPosition :: !Position,
    body :: !(Unify.SchemeOver Body s scope)
  }

instance Unify.Zonk Shared where
  zonk zonker Shared {equalPosition, body} = do
    body <- Unify.zonk zonker body
    pure Shared {equalPosition, body}

data Body s scope = Body
  { definition :: !(RightHandSide s scope),
    typex :: !(Unify.Type s scope)
  }

instance Unify.Zonk Body where
  zonk zonker Body {definition, typex} = do
    definition <- Unify.zonk zonker definition
    typex <- Unify.zonk zonker typex
    pure Body {definition, typex}

instance Unify.Generalizable Body where
  collect collector Body {typex} = Unify.collect collector typex

check :: Context s scopes -> Maybe (Unify.Type s scopes) -> Stage2.Shared scopes -> ST s (Shared s scopes)
check context annotation Stage2.Shared {definition, equalPosition} = case annotation of
  Nothing -> do
    body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      typex <- Unify.fresh Unify.typex
      definition <- RightHandSide.check context typex (shift definition)
      pure $ Body {definition, typex}
    pure $ Shared {body, equalPosition}
  Just typex -> do
    body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      definition <- RightHandSide.check context (shift typex) (shift definition)
      pure $ Body {definition, typex = shift typex}
    pure $ Shared {body, equalPosition}

solve :: Shared s scope -> ST s (Solved.Shared scope)
solve Shared {body, equalPosition} = do
  body <-
    Unify.solveSchemeOver
      (Unify.Solve solveBody)
      equalPosition
      body
  pure Solved.Shared {body, equalPosition}

solveBody :: Position -> Body s scope -> ST s (Solved.Body scope)
solveBody position Body {definition, typex} = do
  definition <- RightHandSide.solve definition
  typex <- Unify.solve position typex
  pure Solved.Body {definition, typex}
