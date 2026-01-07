module Stage1.FreeTypeVariables where

import Stage1.Tree.Marked (Marked)
import Stage1.Variable (VariableIdentifier)

class FreeTypeVariables typex where
  freeTypeVariables :: typex position -> [Marked VariableIdentifier position]
