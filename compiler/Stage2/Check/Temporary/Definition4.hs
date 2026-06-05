module Stage2.Check.Temporary.Definition4 where

import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Link.Term as Term
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import qualified Stage2.Tree.Combinators.Inferred as Inferred
import Stage2.Tree.Definition2 (Inferred)
import qualified Stage2.Tree.Definition4 as Solved
import Stage2.Check.Temporary.Definition3 (Definition3)
import qualified Stage2.Check.Temporary.Definition3 as Definition3
import qualified Stage2.Unify as Unify
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)

data Definition4 locality s scope where
  (:::) ::
    !(Solved.Annotation mark Group Check scope) ->
    !(Unify.SchemeOver (Definition3 mark) s scope) ->
    Definition4 locality s scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality s scope
  (::::) ::
    !(Unify.SchemeOver Types s scope) ->
    !(Unify.Solve s (Simple.SchemeOver (Solved.Set locality Check) scope)) ->
    Definition4 locality s scope

infix 5 :::, ::::

newtype Types s scope = Types (Strict.Vector (Unify.Type s scope))

instance Unify.Zonk Types where
  zonk zonker (Types types) = do
    types <- traverse (Unify.zonk zonker) types
    pure $ Types types

instance Unify.Generalizable Types where
  collect collector (Types types) = foldMap (Unify.collect collector) types

newtype Set locality s scope = Set (Strict.Vector (Element locality s scope))

data Element locality s scope = Element
  { element :: !(Definition3 Inferred s (Scope.GroupTerm ':+ scope)),
    link :: !(Term.Link locality)
  }

solve :: Position -> Definition4 locality s scope -> Unify.Solve s (Solved.Definition4 locality Group Check scope)
solve position = \case
  (annotation ::: definition) -> do
    definition <- Unify.solveSchemeOver (Unify.SolveScheme Definition3.solve) position definition
    pure $ annotation Solved.::: Implicit.Check definition
  Link link id -> pure (Solved.Link link id)
  types :::: set -> do
    types <- Unify.solveSchemeOver (Unify.SolveScheme solveTypes) position types
    set <- set
    pure $ Inferred.Solved types Solved.:::: Implicit.Check set

solveTypes :: Position -> Types s1 scope1 -> Unify.Solve s1 (Solved.Types scope1)
solveTypes position (Types types) = do
  types <- traverse (Unify.solve position) types
  pure $ Solved.Types types

solveGroup :: Position -> Set locality s scope -> Unify.Solve s (Solved.Set locality Check scope)
solveGroup position (Set elements) = do
  elements <- traverse (solveElement position) elements
  pure $ Solved.Set elements

solveElement :: Position -> Element locality s scope -> Unify.Solve s (Solved.Element locality Check scope)
solveElement position Element {element, link} = do
  element <- Definition3.solve position element
  pure Solved.Element {element, link}
