module Semantic.Resolve.Temporary.Complete.Selector where

import qualified Semantic.Tree.Selector as Real
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Selector = Selector
  { name :: !Variable,
    position :: !Position,
    real :: Real.Selector
  }

shrink :: Selector -> Real.Selector
shrink = real
