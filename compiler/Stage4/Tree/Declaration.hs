module Stage4.Tree.Declaration where

import qualified Stage2.Index.Term as Stage2.Term
import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Implicit (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Declaration (Key (..))
import qualified Stage2.Tree.Declaration as Stage3 (Declaration (..))
import qualified Stage2.Tree.Definition2 as Stage3 (Choice (..), Definition2 (Definition, Piece))
import qualified Stage2.Tree.Definition2 as Stage3.Definition2
import qualified Stage2.Tree.Definition3 as Stage3 (Definition3 (..))
import qualified Stage2.Tree.Definition4 as Stage3 (Definition4 (..))
import qualified Stage2.Tree.Expression as Stage3 (Expression)
import qualified Stage3.Tree.Scheme as Stage3 (Scheme)
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Temporary.Pattern as Pattern
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Scheme
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver
import qualified Stage4.Tree.Statements as Statements

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
  forall locality scope.
  Stage3.Declaration locality Normal Check scope ->
  Declaration scope
simplify = \case
  Stage3.Declaration
    { name,
      definition,
      typex
    } ->
      Declaration
        { name,
          definition =
            case definition of
              _ Stage3.::: _ Stage3.::@ Check definition ->
                SchemeOver.map go definition,
          typex = case typex of Solved typex -> typex
        }
  where
    go :: SchemeOver.Map (Stage3.Definition2 source mark Normal Check) Expression
    go = SchemeOver.Map $ \case
      Stage3.Definition definition -> Expression.simplify definition
      Stage3.Piece Stage3.Choice {index, instanciation = Solved instanciation, patternx, bound} ->
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
                          Stage2.Term.Pattern bound
                  }
          }
      Stage3.Definition2.Shared shared -> Expression.simplify shared

annotation ::
  SchemeOver (Stage3.Expression Normal Check) scope ->
  Stage3.Scheme position Check scope ->
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
