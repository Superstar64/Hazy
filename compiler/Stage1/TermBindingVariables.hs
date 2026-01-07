module Stage1.TermBindingVariables where

import Stage1.Tree.Marked (Marked)
import Stage1.Variable (Variable)

class TermBindingVariables declaration where
  termBindingVariables :: declaration position -> [Marked Variable position]
