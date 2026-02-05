module Stage4.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.TermDeclaration as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Scheme (Scheme (Scheme))
import qualified Stage4.Tree.Scheme as Scheme
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver

data TermDeclaration scope = Definition
  { name :: !Variable,
    definition :: !(SchemeOver Expression scope),
    typex :: !(Scheme scope)
  }
  deriving (Show)

instance Shift TermDeclaration where
  shift = shiftDefault

instance Shift.Functor TermDeclaration where
  map = Shift2.mapDefault

instance Shift2.Functor TermDeclaration where
  map = Substitute.mapDefault

instance Substitute.Functor TermDeclaration where
  map category Definition {name, definition, typex} =
    Definition
      { name,
        definition = Substitute.map category definition,
        typex = Substitute.map category typex
      }

simplify :: Stage3.TermDeclaration scope -> TermDeclaration scope
simplify
  Stage3.Manual
    { name,
      definition,
      typex =
        typex@(Scheme SchemeOver {parameters, constraints})
    } =
    Definition
      { name,
        definition =
          SchemeOver
            { parameters,
              constraints,
              result = Expression.simplify definition
            },
        typex
      }
simplify Stage3.Auto {name, definitionAuto, typeAuto} =
  Definition
    { name,
      definition = SchemeOver.mono (Expression.simplify definitionAuto),
      typex = Scheme.mono typeAuto
    }
