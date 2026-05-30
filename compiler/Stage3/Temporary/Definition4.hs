module Stage3.Temporary.Definition4 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import Stage2.Stage (Check)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import qualified Stage2.Tree.Definition4 as Solved
import Stage3.Temporary.Definition3 (Definition3)
import qualified Stage3.Temporary.Definition3 as Definition3
import qualified Stage3.Unify as Unify

data Definition4 s scope where
  (:::) ::
    !(Solved.Annotation mark Normal Check scope) ->
    !(Unify.SchemeOver (Definition3 mark) s scope) ->
    Definition4 s scope

infixr 5 :::

instance Unify.Zonk Definition4 where
  zonk zonker (annotation ::: definition) = do
    definition <- Unify.zonk zonker definition
    pure $ annotation ::: definition

solve :: Position -> Definition4 s scope -> ST s (Solved.Definition4 locality Normal Check scope)
solve position (annotation ::: definition) = do
  definition <- Unify.solveSchemeOver (Unify.Solve Definition3.solve) position definition
  pure $ annotation Solved.::: Implicit.Check definition
