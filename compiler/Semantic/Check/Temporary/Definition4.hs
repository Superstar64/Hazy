module Semantic.Check.Temporary.Definition4 where

import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver)
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Definition2 (Inferred)
import qualified Semantic.Tree.Definition3 as Solved (Definition3)
import qualified Semantic.Tree.Definition4 as Solved
  ( Annotation,
    Definition4 (..),
    Element (..),
    Set,
    Types (..),
  )
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Definition4 locality s scope where
  (:::) ::
    !(Solved.Annotation mark Group Check scope) ->
    !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Definition3 mark Group Check) scope)) ->
    Definition4 locality s scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality s scope
  (::::) ::
    !(Unify.ForallOver Types s scope) ->
    !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Set locality Check) scope)) ->
    Definition4 locality s scope

infix 5 :::, ::::

newtype Types s scope = Types (Strict.Vector (Unify.Type s scope))

instance Unify.Zonk Types where
  zonk zonker (Types types) = do
    types <- traverse (Unify.zonk zonker) types
    pure $ Types types

instance Unify.Generalizable Types where
  collect collector (Types types) = foldMap (Unify.collect collector) types

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
    types <- Unify.solveForallOver (Unify.SolveForall solveTypes) position types
    set <- set
    pure $ Inferred.Solved types Solved.:::: Implicit.Check set

solveTypes :: Position -> Types s1 scope1 -> Unify.Solve s1 (Solved.Types Vacuous scope1)
solveTypes position (Types types) = do
  types <- traverse (Unify.solve position) types
  pure $ Solved.Types types

solveElement :: Element locality s scope -> Unify.Solve s (Solved.Element locality Check scope)
solveElement Element {element, link} = do
  element <- element
  pure Solved.Element {element, link}
