module Stage1.FreeVariables where

import Stage1.Tree.Marked (Marked)
import Stage1.Variable (Variable, VariableIdentifier)

class FreeTypeVariables typex where
  freeTypeVariables :: typex position -> [Marked VariableIdentifier position]

class TermBindingVariables declaration where
  termBindingVariables :: declaration position -> [Marked Variable position]
