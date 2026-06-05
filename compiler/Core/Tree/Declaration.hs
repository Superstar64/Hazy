module Core.Tree.Declaration where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import qualified Core.Temporary.Pattern as Pattern
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import Core.Tree.Scheme (Scheme)
import qualified Core.Tree.Scheme as Scheme
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Core.Tree.SchemeOver as SchemeOver
import qualified Core.Tree.Statements as Statements
import qualified Semantic.Check.Go.Scheme as Semantic (Scheme)
import qualified Semantic.Index.Term as Semantic.Term
import Semantic.Layout (Normal)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Declaration (Key (..))
import qualified Semantic.Tree.Declaration as Semantic (Declaration (..))
import qualified Semantic.Tree.Definition2 as Semantic (Choice (..), Definition2 (Definition, Piece))
import qualified Semantic.Tree.Definition2 as Semantic.Definition2
import qualified Semantic.Tree.Definition3 as Semantic (Definition3 (..))
import qualified Semantic.Tree.Definition4 as Semantic (Definition4 (..))
import qualified Semantic.Tree.Expression as Semantic (Expression)

data Declaration scope = Declaration
  { name :: !Key,
    definition :: SchemeOver Expression scope,
    typex :: Scheme scope
  }
  deriving (Show)

instance Shift Declaration where
  shift = shiftDefault

instance Shift.Functor Declaration where
  map = Shift2.mapDefault

instance Shift2.Functor Declaration where
  map = Substitute.mapDefault

instance Substitute.Functor Declaration where
  map category Declaration {name, definition, typex} =
    Declaration
      { name,
        definition = Substitute.map category definition,
        typex = Substitute.map category typex
      }

simplify ::
  Semantic.Declaration locality Normal Check scope ->
  Declaration scope
simplify = \case
  Semantic.Declaration
    { name,
      definition,
      typex
    } ->
      Declaration
        { name,
          definition =
            case definition of
              _ Semantic.::: Check definition ->
                SchemeOver.map (SchemeOver.Map definition3) definition,
          typex = case typex of Solved typex -> typex
        }
  where
    definition3 :: Semantic.Definition3 mark Normal Check scope -> Expression scope
    definition3 (Semantic.Label _ definition) = definition2 definition

    definition2 :: Semantic.Definition2 source mark Normal Check scope -> Expression scope
    definition2 = \case
      Semantic.Definition definition -> Expression.simplify definition
      Semantic.Piece Semantic.Choice {index, instanciation = Solved instanciation, patternx, bound} ->
        Expression.Join
          { statements =
              Statements.bind
                (Pattern.simplify patternx)
                Expression.Variable
                  { variable = Term.from index,
                    instanciation
                  }
                Statements.Done
                  { done =
                      Expression.monoVariable $
                        Term.from $
                          Semantic.Term.Pattern bound
                  }
          }
      Semantic.Definition2.Shared shared -> Expression.simplify shared

annotation ::
  SchemeOver (Semantic.Expression Normal Check) scope ->
  Semantic.Scheme position Check scope ->
  Declaration scope
annotation SchemeOver {parameters, constraints, result} scheme =
  Declaration
    { name = Unnamed 0,
      definition =
        SchemeOver
          { parameters,
            constraints,
            result = Expression.simplify result
          },
      typex = Scheme.simplify scheme
    }
