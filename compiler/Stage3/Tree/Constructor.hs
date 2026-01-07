module Stage3.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage3.Tree.Entry (Entry)
import Stage3.Tree.Field (Field)

data Constructor scope
  = Constructor
      { entries :: !(Strict.Vector (Entry scope))
      }
  | Record
      { fields :: !(Strict.Vector (Field scope))
      }
  deriving (Show)
