module Stage4.Temporary.TermDeclaration where

import Stage1.Variable (Variable)
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.TermDeclaration as Stage3
import qualified Stage4.Temporary.Definition as Definition
import Stage4.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage4.Temporary.Expression as Expression
import Stage4.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.TermDeclaration as Real

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
        definition = Term.map (Term.over category) definition,
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

finish :: TermDeclaration scope -> Real.TermDeclaration scope
finish Definition {name, definition, typex} =
  Real.Definition
    { name,
      definition = Expression.finish definition,
      typex
    }
