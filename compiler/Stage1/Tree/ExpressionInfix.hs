-- |
-- Parser syntax tree for infix expressions
module Stage1.Tree.ExpressionInfix where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Stage1.Parser (Parser, asum, position, some, token, (<**>), (<|>))
import Stage1.Position (Position)
import Stage1.Tree.Expression (Expression)
import qualified Stage1.Tree.Expression as Expression
import qualified Stage1.Tree.Marked as Marked
import qualified Stage1.Variable as Variable
import Prelude hiding (head, tail)

data Infix position
  = Expression !(Expression position)
  | -- |
    -- > e + e
    Infix
      { left :: !(Expression position),
        operator :: !(Marked.QualifiedName position),
        right :: !(Infix position)
      }
  | -- |
    -- > e : e
    InfixCons
      { head :: !(Expression position),
        operatorPosition :: !position,
        tail :: !(Infix position)
      }
  deriving (Show)

toExpression :: Infix position -> Expression position
toExpression = \case
  Expression e -> e
  Infix {left, operator, right} ->
    Expression.Infix
      { Expression.left,
        Expression.operator,
        Expression.right
      }
  InfixCons {head, operatorPosition, tail} ->
    Expression.InfixCons
      { Expression.head,
        Expression.operatorPosition,
        Expression.tail
      }

parse :: Parser (Infix Position)
parse =
  Expression.parse2
    <**> asum
      [ make <$> position <*> operator <*> parse,
        pure Expression
      ]
  where
    operator = Left <$> Variable.parseOperator <|> Right <$> token ":"
    make position (Left operator) right left =
      Infix
        { left,
          operator = position Marked.:@ operator,
          right
        }
    make operatorPosition (Right ()) tail head =
      InfixCons
        { head,
          operatorPosition,
          tail
        }

parseLeftSection :: Parser (Expression Position)
parseLeftSection = parseLeftSection <$> some ((,) <$> Expression.parse2 <*> operator)
  where
    operator = Left <$> Marked.parseOperator <|> Right <$> (position <* token ":")
    parseLeftSection expressions = case snd $ NonEmpty.last expressions of
      Left operator ->
        Expression.LeftSection
          { Expression.leftSection = infixed expressions,
            Expression.operator
          }
      Right operatorPosition ->
        Expression.LeftSectionCons
          { Expression.leftSection = infixed expressions,
            Expression.operatorPosition
          }
    infixed ((expression, _) :| []) = Expression expression
    infixed ((left, Left operator) :| expression : expressions) =
      Infix
        { left,
          operator,
          right = infixed (expression :| expressions)
        }
    infixed ((head, Right operatorPosition) :| expression : expressions) =
      InfixCons
        { head,
          operatorPosition,
          tail = infixed (expression :| expressions)
        }
