module Stage2.Temporary.Complete.TermDeclaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Temporary.Partial.More.Method as More
import qualified Stage2.Temporary.Partial.More.Selector as More
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.TermDeclaration as Real
import Prelude hiding (Real)

data Real scope
  = Real (Real.TermDeclaration scope)
  | Select !More.Selector
  | Method !More.Method

data TermDeclaration scope
  = TermDeclaration
  { position :: !Position,
    name :: !Variable,
    fixity :: !Fixity,
    annotation :: !(Maybe (Scheme Position scope)),
    declaration :: !(Real scope)
  }
