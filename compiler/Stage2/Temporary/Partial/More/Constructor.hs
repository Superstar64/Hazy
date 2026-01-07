module Stage2.Temporary.Partial.More.Constructor where

import Data.Map (Map)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Variable (Variable)

data Constructor = Constructor
  { typeIndex :: !Int,
    constructorIndex :: !Int,
    fields :: !(Map Variable Int),
    selections :: !(Strict.Vector (Strict.Maybe Int))
  }
