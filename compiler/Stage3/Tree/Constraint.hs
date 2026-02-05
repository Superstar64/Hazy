module Stage3.Tree.Constraint where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage3.Tree.Type (Type)

data Constraint scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type (Local ':+ scope)))
  }
  deriving (Show)
