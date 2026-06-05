module Stage2.Check.Temporary.Definition2 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Index.Table.Term as Term ((!))
import Stage2.Index.Term (Bound)
import qualified Stage2.Index.Term as Term (Index)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Inferred as Inferred
import Stage2.Tree.Definition2 (Annotated, Inferred, Share, Single)
import qualified Stage2.Tree.Definition2 as Solved (Choice (..), Definition2 (..))
import qualified Stage2.Tree.Definition2 as Stage2
import Stage2.Check.Context (Context (..))
import Stage2.Check.TermBinding (TermBinding (TermBinding), Type (..))
import Stage2.Check.Simple.Scheme (instanciate)
import Stage2.Check.Temporary.Definition (Definition)
import qualified Stage2.Check.Temporary.Definition as Definition
import Stage2.Check.Temporary.Pattern (Pattern)
import qualified Stage2.Check.Temporary.Pattern as Pattern
import Stage2.Check.Temporary.RightHandSide (RightHandSide)
import qualified Stage2.Check.Temporary.RightHandSide as RightHandSide
import qualified Stage2.Unify as Unify

data Definition2 source mark s scope where
  Definition :: !(Definition s scope) -> Definition2 Single mark s scope
  Piece :: !(Choice s scope) -> Definition2 Single mark s scope
  Shared :: !(RightHandSide s scope) -> Definition2 Share Inferred s scope

data Choice s scope = Choice
  { index :: !(Term.Index scope),
    instanciation :: !(Unify.Instanciation s scope),
    patternx :: !(Pattern s scope),
    bound :: !Bound
  }

checkManual ::
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Stage2.Definition2 source Annotated Group Resolve scopes ->
  ST s (Definition2 source Annotated s (Local ':+ scopes))
checkManual context typex (Stage2.Definition definition) = do
  definition <- Definition.check context typex (shift definition)
  pure $ Definition definition
checkManual context typex (Stage2.Scoped definition) = do
  definition <- Definition.check context typex definition
  pure $ Definition definition
checkManual context@Context {termEnvironment} typex declaration = case declaration of
  Stage2.Piece Stage2.Choice {position, index, patternx, bound} -> do
    let TermBinding binding = termEnvironment Term.! shift index
    (full, instanciation) <-
      binding >>= \case
        Wobbly typex -> Unify.instanciate context position typex
        Rigid scheme -> instanciate context position scheme
    patternx <- Pattern.check context full (shift patternx)
    let typex' = patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure $ Piece Choice {index = shift index, instanciation, patternx, bound}

checkAuto ::
  Context s scopes ->
  Unify.Type s scopes ->
  Stage2.Definition2 source Inferred Group Resolve scopes ->
  ST s (Definition2 source Inferred s scopes)
checkAuto context typex (Stage2.Definition definition) = do
  definition <- Definition.check context typex definition
  pure $ Definition definition
checkAuto context typex (Stage2.Shared definition) = do
  definition <- RightHandSide.check context typex definition
  pure $ Shared definition
checkAuto context@Context {termEnvironment} typex declaration = case declaration of
  Stage2.Piece Stage2.Choice {position, index, patternx, bound} -> do
    let TermBinding binding = termEnvironment Term.! index
    (full, instanciation) <-
      binding >>= \case
        Wobbly typex -> Unify.instanciate context position typex
        Rigid scheme -> instanciate context position scheme
    patternx <- Pattern.check context full patternx
    let typex' = patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure $ Piece Choice {index, instanciation, patternx, bound}

solve ::
  Position ->
  Definition2 source mark s scope ->
  Unify.Solve s (Solved.Definition2 source mark Group Check scope)
solve position = \case
  Definition definition -> do
    definition <- Definition.solve definition
    pure $ Solved.Definition definition
  Piece Choice {index, instanciation, patternx, bound} -> do
    instanciation <- Unify.solveInstanciation position instanciation
    patternx <- Pattern.solve patternx
    pure $
      Solved.Piece
        Solved.Choice
          { position,
            index,
            instanciation = Inferred.Solved instanciation,
            patternx,
            bound
          }
  Shared shared -> do
    shared <- RightHandSide.solve shared
    pure $ Solved.Shared shared
