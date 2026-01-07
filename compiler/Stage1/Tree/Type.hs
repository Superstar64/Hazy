{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for types
module Stage1.Tree.Type where

import Data.Foldable (toList)
import qualified Data.Strict.Vector2 as Strict (Vector2, fromList'')
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.FreeTypeVariables (FreeTypeVariables (..))
import Stage1.Parser
  ( Parser,
    asum,
    betweenBrackets,
    betweenParens,
    optional,
    position,
    sepByComma,
    some,
    token,
    try,
    (<**>),
    (<|>),
  )
import Stage1.Position (Position)
import Stage1.Printer (Printed)
import qualified Stage1.Printer as Printer
import Stage1.Tree.Marked (Marked ((:@)))
import qualified Stage1.Tree.Marked as Marked
import {-# SOURCE #-} Stage1.Tree.TypeInfix (Infix)
import {-# SOURCE #-} qualified Stage1.Tree.TypeInfix as TypeInfix
import Prelude hiding (head, print, tail)

data Type position
  = -- |
    -- > x
    Variable
      { startPosition :: !position,
        variable :: !(Marked.VariableIdentifier position)
      }
  | -- |
    -- > C
    Constructor
      { startPosition :: !position,
        constructor :: !(Marked.QualifiedConstructorIdentifier position)
      }
  | -- |
    -- > ()
    Unit
      { startPosition :: !position
      }
  | -- |
    -- > (->)
    Arrow
      { startPosition :: !position
      }
  | -- |
    -- > []
    Listing
      { startPosition :: !position
      }
  | -- |
    -- > (,,)
    Tupling
      { startPosition :: !position,
        count :: !Int
      }
  | -- |
    -- > [t]
    List
      { startPosition :: !position,
        element :: !(Type position)
      }
  | -- |
    -- > (t, t)
    Tuple
      { startPosition :: !position,
        elements :: !(Strict.Vector2 (Type position))
      }
  | -- |
    -- > t t
    Call
      { startPosition :: !position,
        function :: !(Type position),
        argument :: !(Type position)
      }
  | -- |
    -- > t -> t
    Function
      { startPosition :: !position,
        parameter :: !(Type position),
        operatorPosition :: !position,
        result :: !(Type position)
      }
  | -- |
    -- > !t -> t
    StrictFunction
      { startPosition :: !position,
        parameter :: !(Type position),
        operatorPosition :: !position,
        result :: !(Type position)
      }
  | -- |
    -- > 'C
    Lifted
      { startPosition :: !position,
        lifted :: !(Marked.QualifiedConstructor position)
      }
  | -- |
    -- > '(:)
    LiftedCons
      { startPosition :: !position
      }
  | -- |
    -- > '[t]
    LiftedList
      { startPosition :: !position,
        items :: !(Strict.Vector (Type position))
      }
  | -- |
    -- > t ':+ t
    Infix
      { startPosition :: !position,
        left :: !(Type position),
        operator :: !(Marked.QualifiedConstructor position),
        right :: !(Infix position)
      }
  | -- |
    -- > t ': t
    InfixCons
      { startPosition :: !position,
        head :: !(Type position),
        operatorPosition :: !position,
        tail :: !(Infix position)
      }
  | -- |
    -- > type t
    Type
      { startPosition :: !position,
        universe :: !(Type position)
      }
  | -- |
    -- > *
    Star
      { startPosition :: !position
      }
  deriving (Show)

instance FreeTypeVariables Type where
  freeTypeVariables = \case
    Variable {variable} -> [variable]
    Constructor {} -> []
    Unit {} -> []
    Arrow {} -> []
    Listing {} -> []
    Tupling {} -> []
    List {element} -> freeTypeVariables element
    Tuple {elements} -> concatMap freeTypeVariables elements
    Call {function, argument} -> freeTypeVariables function ++ freeTypeVariables argument
    Function {parameter, result} -> freeTypeVariables parameter ++ freeTypeVariables result
    StrictFunction {parameter, result} -> freeTypeVariables parameter ++ freeTypeVariables result
    Lifted {} -> mempty
    LiftedCons {} -> mempty
    LiftedList {items} -> foldMap freeTypeVariables items
    Infix {left, right} -> freeTypeVariables left ++ freeTypeVariables right
    InfixCons {head, tail} -> freeTypeVariables head ++ freeTypeVariables tail
    Type {universe} -> freeTypeVariables universe
    Star {} -> []

-- type
parse :: Parser (Type Position)
parse =
  asum
    [ position <**> try (strictFunction <$ token "!" <*> parse3 <*> position <* token "->") <*> parse,
      arrow <$> parse1 <*> position <*> optional (token "->" *> parse)
    ]
  where
    strictFunction parameter operatorPosition startPosition result =
      StrictFunction {startPosition, parameter, operatorPosition, result}
    arrow typex _ Nothing = typex
    arrow parameter operatorPosition (Just result) =
      Function
        { startPosition = startPosition parameter,
          parameter,
          operatorPosition,
          result
        }

print :: Type unit -> Printed
print = \case
  StrictFunction {parameter, result} -> mconcat [Printer.token "!", print3 parameter, Printer.token "->", print result]
  Function {parameter, result} -> mconcat [print1 parameter, Printer.token "->", print result]
  typex -> print1 typex

-- infixed
parse1 :: Parser (Type Position)
parse1 = TypeInfix.toType <$> TypeInfix.parse

print1 :: Type unit -> Printed
print1 = \case
  Infix {left, operator, right} -> mconcat [print2 left, Marked.printOperator operator, TypeInfix.print right]
  InfixCons {head, tail} -> mconcat [print2 head, Printer.token ":", TypeInfix.print tail]
  typex -> print2 typex

-- btype
parse2 :: Parser (Type Position)
parse2 = typex <$ token "type" <*> position <*> parse3 <|> foldl1 call <$> some parse3
  where
    typex startPosition universe = Type {startPosition, universe}
    call function argument = Call {startPosition = startPosition function, function, argument}

print2 :: Type unit -> Printed
print2 = \case
  Type {universe} -> mconcat [Printer.token "type", print3 universe]
  Call {function, argument} -> mconcat [print2 function, print3 argument]
  typex -> print3 typex

-- atype
parse3 :: Parser (Type Position)
parse3 =
  asum
    [ variable <$> Marked.parse,
      constructor <$> Marked.parse,
      unit <$> position <* try (betweenParens (pure ())),
      arrow <$> position <* try (betweenParens (token "->")),
      tuple <$> position <*> betweenParens (Left <$> some (token ",") <|> Right <$> sepByComma parse),
      listing <$> position <* try (betweenBrackets (pure ())),
      list <$> position <*> betweenBrackets parse,
      star <$> position <* token "*",
      try $
        token "'"
          *> asum
            [ lifted <$> Marked.parseLiteral,
              liftedCons <$> position <* betweenParens (token ":"),
              liftedList <$> position <*> betweenBrackets (Strict.Vector.fromList <$> sepByComma parse)
            ]
    ]
  where
    variable variable@(startPosition :@ _) = Variable {startPosition, variable}
    constructor constructor@(startPosition :@ _) = Constructor {startPosition, constructor}
    unit startPosition = Unit {startPosition}
    arrow startPosition = Arrow {startPosition}
    list startPosition element = List {startPosition, element}
    lifted lifted@(startPosition :@ _) = Lifted {startPosition, lifted}
    liftedList startPosition items = LiftedList {startPosition, items}
    listing startPosition = Listing {startPosition}
    liftedCons startPosition = LiftedCons {startPosition}
    tuple startPosition (Left tupling) = Tupling {startPosition, count = length tupling}
    tuple startPosition (Right []) = Unit {startPosition}
    tuple _ (Right [e]) = e
    tuple startPosition (Right (e : e1 : es)) = Tuple {startPosition, elements = Strict.fromList'' e e1 es}
    star startPosition = Star {startPosition}

print3 :: Type unit -> Printed
print3 = \case
  Variable {variable} -> Marked.print variable
  Constructor {constructor} -> Marked.print constructor
  Unit {} -> mconcat [Printer.token "(", Printer.token ")"]
  Arrow {} -> mconcat [Printer.token "(", Printer.token "->", Printer.token ")"]
  Tuple {elements} -> mconcat [Printer.token "(", interleave (toList elements), Printer.token ")"]
  Tupling {count} ->
    mconcat $ [Printer.token "(", mconcat $ replicate count (Printer.token ","), Printer.token ")"]
  List {element} -> mconcat [Printer.token "[", print element, Printer.token "]"]
  Listing {} -> mconcat [Printer.token "[", Printer.token "]"]
  Lifted {lifted} -> mconcat [Printer.token "'", Marked.printLiteral lifted]
  LiftedCons {} -> mconcat [Printer.token "'", Printer.token ":"]
  LiftedList {items} -> mconcat [Printer.token "'", Printer.token "[", interleave (toList items), Printer.token "]"]
  Star {} -> Printer.token "*"
  typex -> mconcat [Printer.token "(", print typex, Printer.token ")"]
  where
    interleave [] = mempty
    interleave (head : tail) = print head <> foldMap ((<>) (Printer.token ",") . print) tail
