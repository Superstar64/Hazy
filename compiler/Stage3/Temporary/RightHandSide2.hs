module Stage3.Temporary.RightHandSide2 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Tree.RightHandSide2 as Solved
import qualified Stage3.Unify as Unify

data RightHandSide2 s scope = RightHandSide2
  { definition :: !(RightHandSide s scope),
    typex :: !(Unify.Type s scope)
  }

instance Unify.Zonk RightHandSide2 where
  zonk zonker RightHandSide2 {definition, typex} = do
    definition <- Unify.zonk zonker definition
    typex <- Unify.zonk zonker typex
    pure RightHandSide2 {definition, typex}

instance Unify.Generalizable RightHandSide2 where
  collect collector RightHandSide2 {typex} = Unify.collect collector typex

solve :: Position -> RightHandSide2 s scope -> ST s (Solved.RightHandSide2 scope)
solve position RightHandSide2 {definition, typex} = do
  definition <- RightHandSide.solve definition
  typex <- Unify.solve position typex
  pure Solved.RightHandSide2 {definition, typex}
