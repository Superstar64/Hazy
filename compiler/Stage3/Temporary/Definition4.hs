module Stage3.Temporary.Definition4 where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Link.Term as Term
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import qualified Stage2.Tree.Combinators.Inferred as Combinators
import Stage2.Tree.Definition2 (Inferred)
import qualified Stage2.Tree.Definition4 as Solved
import Stage3.Temporary.Definition3 (Definition3)
import qualified Stage3.Temporary.Definition3 as Definition3
import qualified Stage3.Unify as Unify

data Definition4 locality s scope where
  (:::) ::
    !(Solved.Annotation mark Group Check scope) ->
    !(Unify.SchemeOver (Definition3 mark) s scope) ->
    Definition4 locality s scope
  Link :: !(Term.Link locality) -> !Int -> Definition4 locality s scope
  Group :: !(Unify.SchemeOver (Set locality) s scope) -> Definition4 locality s scope

infixr 5 :::

instance Unify.Zonk (Definition4 locality) where
  zonk zonker = \case
    (annotation ::: definition) -> do
      definition <- Unify.zonk zonker definition
      pure $ annotation ::: definition
    Link link id -> pure (Link link id)
    Group set -> Group <$> Unify.zonk zonker set

newtype Set locality s scope = Set (Strict.Vector (Element locality s scope))

instance Unify.Zonk (Set locality) where
  zonk zonker (Set set) = Set <$> traverse (Unify.zonk zonker) set

instance Unify.Generalizable (Set locality) where
  collect collector (Set set) = foldMap (Unify.collect collector . typex) set

data Element locality s scope = Element
  { element :: !(Definition3 Inferred s (Scope.GroupTerm ':+ scope)),
    typex :: !(Unify.Type s scope),
    link :: !(Term.Link locality)
  }

instance Unify.Zonk (Element locality) where
  zonk zonker Element {element, typex, link} = do
    element <- Unify.zonk zonker element
    typex <- Unify.zonk zonker typex
    pure Element {element, typex, link}

solve :: Position -> Definition4 locality s scope -> ST s (Solved.Definition4 locality Group Check scope)
solve position = \case
  (annotation ::: definition) -> do
    definition <- Unify.solveSchemeOver (Unify.Solve Definition3.solve) position definition
    pure $ annotation Solved.::: Implicit.Check definition
  Link link id -> pure (Solved.Link link id)
  Group set -> do
    set <- Unify.solveSchemeOver (Unify.Solve solveGroup) position set
    pure $ Solved.Group $ Implicit.Check set

solveGroup :: Position -> Set locality s scope -> ST s (Solved.Set locality Check scope)
solveGroup position (Set elements) = do
  elements <- traverse (solveElement position) elements
  pure $ Solved.Set elements

solveElement :: Position -> Element locality s scope -> ST s (Solved.Element locality Check scope)
solveElement position Element {element, typex, link} = do
  element <- Definition3.solve position element
  typex <- Unify.solve position typex
  pure Solved.Element {element, typex = Combinators.Solved typex, link}
