module Stage4.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import qualified Stage2.Index.Term as Stage2.Term
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Shared as Stage3.Shared
import qualified Stage3.Tree.TermDeclaration as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Temporary.Pattern as Pattern
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Scheme (Scheme (Scheme))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.Statements as Statements

data Name
  = Name !Variable
  | Unnamed !Int
  deriving (Show)

data LazyTermDeclaration scope
  = !Name :^ TermDeclaration scope
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
  { name :: !Name,
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

simplify :: (Int -> Term.Index scope) -> Stage3.LazyTermDeclaration scope -> LazyTermDeclaration scope
simplify share (name Stage3.:^ declaration) =
  Name name :^ case Stage3.body declaration of
    SchemeOver
      { parameters,
        constraints,
        result
      } ->
        Definition
          { name = Name $ Stage3.name declaration,
            definition =
              SchemeOver
                { parameters,
                  constraints,
                  result = case result of
                    Stage3.Body {definition} -> Expression.simplify definition
                    Stage3.Shared {shareIndex, instanciation, patternx, bound} ->
                      Expression.Join
                        { statements =
                            Statements.bind
                              (Pattern.simplify patternx)
                              Expression.Variable
                                { variable = shift $ share shareIndex,
                                  instanciation
                                }
                              Statements.Done
                                { done =
                                    Expression.monoVariable $
                                      Term.from $
                                        Stage2.Term.Pattern bound
                                }
                        }
                },
            typex =
              Scheme
                SchemeOver
                  { parameters,
                    constraints,
                    result = Stage3.typex result
                  }
          }

simplifyShared :: Int -> Stage3.Shared.Shared scope -> LazyTermDeclaration scope
simplifyShared index shared =
  Unnamed index :^ case shared of
    Stage3.Shared.Shared {body} -> case body of
      SchemeOver {parameters, constraints, result} ->
        Definition
          { name = Unnamed index,
            definition =
              SchemeOver
                { parameters,
                  constraints,
                  result = Expression.simplify $ Stage3.Shared.definition result
                },
            typex =
              Scheme
                SchemeOver
                  { parameters,
                    constraints,
                    result = Stage3.Shared.typex result
                  }
          }
