module Stage3.Temporary.Definition4 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage3.Temporary.Definition3 (Definition3)
import qualified Stage3.Temporary.Definition3 as Definition3
import qualified Stage3.Tree.Definition4 as Solved
import qualified Stage3.Unify as Unify

data Definition4 s scope where
  (:::) :: !(Solved.Annotation mark scope) -> !(Definition3 mark s scope) -> Definition4 s scope

infixr 5 :::

instance Unify.Zonk Definition4 where
  zonk zonker (annotation ::: definition) = do
    definition <- Unify.zonk zonker definition
    pure $ annotation ::: definition

solve :: Position -> Definition4 s scope -> ST s (Solved.Definition4 scope)
solve position (annotation ::: definition) = do
  definition <- Definition3.solve position definition
  pure $ annotation Solved.::: definition
