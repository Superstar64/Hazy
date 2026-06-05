{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for infix patterns
module Syntax.Tree.PatternInfix where

import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    position,
    token,
    (<**>),
    (<|>),
  )
import Syntax.Position (Position)
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.Pattern (Pattern)
import qualified Syntax.Tree.Pattern as Pattern

data Infix position
  = Pattern
      { startPosition :: !position,
        patternx :: !(Pattern position)
      }
  | -- |
    -- > x :+ x
    Infix
      { startPosition :: !position,
        left :: !(Pattern position),
        operator :: !(Marked.QualifiedConstructor position),
        operatorPosition :: !position,
        right :: !(Infix position)
      }
  | -- |
    -- > x : x
    InfixCons
      { startPosition :: !position,
        left :: !(Pattern position),
        operatorPosition :: !position,
        right :: !(Infix position)
      }
  deriving (Show)

toPattern :: Infix position -> Pattern position
toPattern = \case
  Pattern {patternx} -> patternx
  Infix {startPosition, left, operator, operatorPosition, right} ->
    Pattern.Infix
      { startPosition,
        left,
        operator,
        operatorPosition,
        right
      }
  InfixCons {startPosition, left, operatorPosition, right} ->
    Pattern.InfixCons
      { startPosition,
        left,
        operatorPosition,
        right
      }

instance TermBindingVariables Infix where
  termBindingVariables = \case
    Pattern {patternx} -> termBindingVariables patternx
    Infix {left, right} -> termBindingVariables left ++ termBindingVariables right
    InfixCons {left, right} -> termBindingVariables left ++ termBindingVariables right

parse :: Parser (Infix Position)
parse =
  Pattern.parse1
    <**> asum
      [ make <$> position <*> operator <*> parse,
        pure patternx
      ]
  where
    patternx patternx =
      Pattern
        { startPosition = Pattern.startPosition patternx,
          patternx
        }
    operator = Left <$> Marked.parseOperator <|> Right <$> token ":"
    make operatorPosition (Left operator) right left =
      Infix
        { startPosition = Pattern.startPosition left,
          left,
          operator,
          operatorPosition,
          right
        }
    make operatorPosition (Right ()) right left =
      InfixCons
        { startPosition = Pattern.startPosition left,
          left,
          operatorPosition,
          right
        }
