module Semantic.Check.Temporary.Definition4 where

import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver)
import Data.Void (Void)
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Definition2 (Inferred)
import qualified Semantic.Tree.Definition3 as Solved (Definition3)
import Semantic.Tree.Definition4 (Types (..))
import qualified Semantic.Tree.Definition4 as Solved
  ( Annotation,
    Definition4 (..),
    Element (..),
    Set,
    Types (..),
  )
import Semantic.Unify (Logical)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Definition4 locality s scope where
  (:::) ::
    !(Solved.Annotation mark Group Check scope) ->
    !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Definition3 mark Group Check) scope)) ->
    Definition4 locality s scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality s scope
  (::::) ::
    !(Unify.ForallOver Types (Logical s scope) scope) ->
    !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Set locality Group Check) scope)) ->
    Definition4 locality s scope

infix 5 :::, ::::

data Element locality s scope = Element
  { element :: !(Unify.Solve s (Solved.Definition3 Inferred Group Check (Scope.GroupTerm ':+ scope))),
    link :: !(Term.Link locality)
  }

solve :: Position -> Definition4 locality s scope -> Unify.Solve s (Solved.Definition4 locality Group Check scope)
solve position = \case
  (annotation ::: definition) -> do
    definition <- definition
    pure $ annotation Solved.::: Implicit.Check definition
  Link link id -> pure (Solved.Link link id)
  types :::: set -> do
    types <- Unify.solve position types
    set <- set
    pure $ Inferred.Solved types Solved.:::: Implicit.Check set

solveTypes :: Position -> Types (Logical s scope) scope -> Unify.Solve s (Solved.Types Void scope)
solveTypes position (Types types) = do
  types <- traverse (Unify.solve position) types
  pure $ Solved.Types types

solveElement :: Element locality s scope -> Unify.Solve s (Solved.Element locality Group Check scope)
solveElement Element {element, link} = do
  element <- element
  pure Solved.Element {element, link}
