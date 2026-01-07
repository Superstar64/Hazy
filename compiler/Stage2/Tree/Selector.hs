module Stage2.Tree.Selector where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Variable (Variable)

data Selector
  = Selector
  { name :: !Variable,
    position :: !Position,
    first, index :: !Int,
    uniform :: !Uniform
  }
  deriving (Show)

data Uniform
  = Uniform
      { strict :: !Bool
      }
  | Disjoint
      { indexes :: !(Strict.Vector (Strict.Maybe Int))
      }
  deriving (Show)
