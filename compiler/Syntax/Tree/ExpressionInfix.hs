{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for infix expressions
module Syntax.Tree.ExpressionInfix where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Syntax.Parser (Parser, asum, position, some, token, (<**>), (<|>))
import Syntax.Position (Position)
import Syntax.Tree.Expression (Expression)
import qualified Syntax.Tree.Expression as Expression
import qualified Syntax.Tree.Marked as Marked
import qualified Syntax.Variable as Variable
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
  | -- |
    -- > - e
    Negate
      { startPosition :: !position,
        negative :: !(Infix position)
      }
  deriving (Show)

toExpression :: Infix position -> Expression position
toExpression = \case
  Expression e -> e
  Infix {left, operator, right} ->
    Expression.Infix
      { left,
        operator,
        right
      }
  InfixCons {head, operatorPosition, tail} ->
    Expression.InfixCons
      { head,
        operatorPosition,
        tail
      }
  Negate {startPosition, negative} -> Expression.Negate {startPosition, negative}

parse :: Parser (Infix Position)
parse =
  Expression.parse2
    <**> asum
      [ make <$> position <*> operator <*> parse,
        pure Expression
      ]
    <|> negate
    <$> position
    <*> (token "-" *> parse)
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
    negate startPosition negative = Negate {startPosition, negative}

parseLeftSection :: Parser (Expression Position)
parseLeftSection = parseLeftSection <$> some ((,) <$> Expression.parse2 <*> operator)
  where
    operator = Left <$> Marked.parseOperator <|> Right <$> (position <* token ":")
    parseLeftSection expressions = case snd $ NonEmpty.last expressions of
      Left operator ->
        Expression.LeftSection
          { leftSection = infixed expressions,
            operator
          }
      Right operatorPosition ->
        Expression.LeftSectionCons
          { leftSection = infixed expressions,
            operatorPosition
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
