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
import Stage4.Tree.SchemeOver (SchemeOver (..))

data LazyTermDeclaration scope = !Variable :^ TermDeclaration scope
  deriving (Show)

infix 4 :^

instance Shift LazyTermDeclaration where
  shift = shiftDefault

instance Shift.Functor LazyTermDeclaration where
  map = Shift2.mapDefault

instance Shift2.Functor LazyTermDeclaration where
  map = Substitute.mapDefault

instance Substitute.Functor LazyTermDeclaration where
  map category (name :^ declaration) = name :^ Substitute.map category declaration

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

simplify :: Stage3.LazyTermDeclaration scope -> LazyTermDeclaration scope
simplify (name Stage3.:^ declaration) =
  name :^ case Stage3.body declaration of
    SchemeOver
      { parameters,
        constraints,
        result = Stage3.Body {definition, typex}
      } ->
        Definition
          { name = Stage3.name declaration,
            definition =
              SchemeOver
                { parameters,
                  constraints,
                  result = Expression.simplify definition
                },
            typex =
              Scheme
                SchemeOver
                  { parameters,
                    constraints,
                    result = typex
                  }
          }
