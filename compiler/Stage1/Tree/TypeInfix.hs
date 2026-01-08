{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for infix types
module Stage1.Tree.TypeInfix where

import Stage1.FreeTypeVariables (FreeTypeVariables (..))
import Stage1.Parser
  ( Parser,
    asum,
    position,
    token,
    (<**>),
    (<|>),
  )
import Stage1.Position (Position)
import Stage1.Printer (Printed)
import qualified Stage1.Printer as Printer
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Type (Type)
import qualified Stage1.Tree.Type as Type
import qualified Stage1.Variable as Variable
import Prelude hiding (head, print, tail)

data Infix position
  = Type !(Type position)
  | -- |
    -- > t ':+ t
    Infix
      { startPosition :: !position,
        left :: !(Type position),
        operator :: !(Marked.QualifiedConstructor position),
        right :: !(Infix position)
      }
  | -- |
    -- > t : t
    InfixCons
      { startPosition :: !position,
        head :: !(Type position),
        operatorPosition :: !position,
        tail :: !(Infix position)
      }
  deriving (Show)

toType :: Infix Position -> Type Position
toType = \case
  Type typex -> typex
  Infix {startPosition, left, operator, right} ->
    Type.Infix
      { startPosition,
        left,
        operator,
        right
      }
  InfixCons {startPosition, head, operatorPosition, tail} ->
    Type.InfixCons
      { startPosition,
        head,
        operatorPosition,
        tail
      }

instance FreeTypeVariables Infix where
  freeTypeVariables = \case
    Type typex -> freeTypeVariables typex
    Infix {left, right} -> freeTypeVariables left <> freeTypeVariables right
    InfixCons {head, tail} -> freeTypeVariables head <> freeTypeVariables tail

parse :: Parser (Infix Position)
parse =
  Type.parse2
    <**> asum
      [ make <$ token "'" <*> position <*> operator <*> parse,
        pure Type
      ]
  where
    operator = Left <$> Variable.parseOperator <|> Right <$> token ":"
    make operatorPosition (Left operator) right left =
      Infix
        { startPosition = Type.startPosition left,
          left,
          operator = operatorPosition Marked.:@ operator,
          right
        }
    make operatorPosition (Right ()) tail head =
      InfixCons
        { startPosition = Type.startPosition head,
          head,
          operatorPosition,
          tail
        }

print :: Infix unit -> Printed
print = \case
  Infix {left, operator, right} -> mconcat [Type.print2 left, Marked.printOperator operator, print right]
  InfixCons {head, tail} -> mconcat [Type.print2 head, Printer.token ":", print tail]
  Type typex -> Type.print2 typex
