module Stage2.Temporary.Complete.Selector where

import Stage1.Position (Position)
import Stage1.Variable (Variable)
import qualified Stage2.Tree.Selector as Real

data Selector = Selector
  { name :: !Variable,
    position :: !Position,
    real :: Real.Selector
  }

shrink :: Selector -> Real.Selector
shrink = real
