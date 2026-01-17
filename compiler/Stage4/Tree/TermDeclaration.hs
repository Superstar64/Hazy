module Stage4.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.TermDeclaration as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Temporary.Definition as Definition
import Stage4.Tree.Expression (Expression)
import Stage4.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Scheme

data TermDeclaration scope = Definition
  { name :: !Variable,
    definition :: !(Expression (Scope.Local ':+ scope)),
    typex :: !(Scheme scope)
  }
  deriving (Show)

instance Shift TermDeclaration where
  shift = shiftDefault

instance Shift.Functor TermDeclaration where
  map = Term.mapDefault

instance Term.Functor TermDeclaration where
  map category Definition {name, definition, typex} =
    Definition
      { name,
        definition = Term.map (Term.Over category) definition,
        typex = Term.map category typex
      }

simplify :: Stage3.TermDeclaration scope -> TermDeclaration scope
simplify Stage3.Manual {name, definition, typex} =
  Definition
    { name,
      definition = Definition.desugar $ Definition.simplify definition,
      typex
    }
simplify Stage3.Auto {name, definitionAuto, typeAuto} =
  Definition
    { name,
      definition = shift $ Definition.desugar $ Definition.simplify definitionAuto,
      typex = Scheme.mono typeAuto
    }
