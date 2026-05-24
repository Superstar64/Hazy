module Stage3.Temporary.Definition2 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Index.Table.Term as Term ((!))
import Stage2.Index.Term (Bound)
import qualified Stage2.Index.Term as Term (Index)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Inferred as Inferred
import Stage2.Tree.Definition2 (Annotated, Inferred, Share, Single)
import qualified Stage2.Tree.Definition2 as Solved (Choice (..), Definition2 (..))
import qualified Stage2.Tree.Definition2 as Stage2
import Stage3.Check.Context (Context (..))
import Stage3.Check.TermBinding (TermBinding (TermBinding), Type (..))
import Stage3.Simple.Scheme (instanciate)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Unify as Unify

data Definition2 source mark s scope where
  Definition :: !(Definition s scope) -> !(Unify.Type s scope) -> Definition2 Single mark s scope
  Piece :: !(Choice s scope) -> !(Unify.Type s scope) -> Definition2 Single mark s scope
  Shared :: !(RightHandSide s scope) -> !(Unify.Type s scope) -> Definition2 Share Inferred s scope

instance Unify.Zonk (Definition2 source mark) where
  zonk zonker = \case
    Definition definition typex -> do
      definition <- Unify.zonk zonker definition
      typex <- Unify.zonk zonker typex
      pure $ Definition definition typex
    Piece choice typex -> do
      choice <- Unify.zonk zonker choice
      typex <- Unify.zonk zonker typex
      pure $ Piece choice typex
    Shared shared typex -> do
      shared <- Unify.zonk zonker shared
      typex <- Unify.zonk zonker typex
      pure $ Shared shared typex

instance Unify.Generalizable (Definition2 source mark) where
  collect collector body = Unify.collect collector (typex body)

typex :: Definition2 source mark s scope -> Unify.Type s scope
typex = \case
  Definition _ typex -> typex
  Piece _ typex -> typex
  Shared _ typex -> typex

data Choice s scope = Choice
  { index :: !(Term.Index scope),
    instanciation :: !(Unify.Instanciation s scope),
    patternx :: !(Pattern s scope),
    bound :: !Bound
  }

instance Unify.Zonk Choice where
  zonk zonker Choice {index, instanciation, patternx, bound} = do
    instanciation <- Unify.zonk zonker instanciation
    patternx <- Unify.zonk zonker patternx
    pure Choice {index, instanciation, patternx, bound}

data Which mark scope where
  Manual :: Which Annotated (Local ':+ scope)
  Auto :: Which Inferred scope

check ::
  Context s (scope ':+ scopes) ->
  Which mark (scope ':+ scopes) ->
  Unify.Type s (scope ':+ scopes) ->
  Stage2.Definition2 source mark Normal Resolve scopes ->
  ST s (Definition2 source mark s (scope ':+ scopes))
check context Auto typex (Stage2.Definition definition) = do
  definition <- Definition.check context typex (shift definition)
  pure $ Definition definition typex
check context Manual typex (Stage2.Definition definition) = do
  definition <- Definition.check context typex (shift definition)
  pure $ Definition definition typex
check context Manual typex (Stage2.Scoped definition) = do
  definition <- Definition.check context typex definition
  pure $ Definition definition typex
check context Auto typex (Stage2.Shared definition) = do
  definition <- RightHandSide.check context typex (shift definition)
  pure $ Shared definition typex
check context@Context {termEnvironment} _ typex declaration = case declaration of
  Stage2.Piece Stage2.Choice {position, index, patternx, bound} -> do
    let TermBinding binding = termEnvironment Term.! shift index
    (full, instanciation) <-
      binding >>= \case
        Wobbly typex -> Unify.instanciate context position (Unify.monoScheme typex)
        Rigid scheme -> instanciate context position scheme
    patternx <- Pattern.check context full (shift patternx)
    let typex' = patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure $ Piece Choice {index = shift index, instanciation, patternx, bound} typex

solve ::
  Position ->
  Definition2 source mark s scope ->
  ST s (Solved.Definition2 source mark Normal Check scope)
solve position = \case
  Definition definition _ -> do
    definition <- Definition.solve definition
    pure $ Solved.Definition definition
  Piece Choice {index, instanciation, patternx, bound} _ -> do
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
  Shared shared _ -> do
    shared <- RightHandSide.solve shared
    pure $ Solved.Shared shared
