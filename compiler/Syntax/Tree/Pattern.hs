{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for patterns
module Syntax.Tree.Pattern where

import qualified Control.Applicative as Applicative
import qualified Data.Strict.Vector2 as Strict (Vector2, fromList'')
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    StringLiteral,
    asum,
    betweenBraces,
    betweenBrackets,
    betweenParens,
    char,
    decimal,
    position,
    sepByComma,
    text,
    token,
    try,
    (<**>),
  )
import qualified Syntax.Parser as Parser
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Marked as Marked
import {-# SOURCE #-} Syntax.Tree.PatternInfix (Infix)
import {-# SOURCE #-} qualified Syntax.Tree.PatternInfix as PatternInfix
import Syntax.Variable
  ( QualifiedConstructor,
    QualifiedVariable (..),
  )
import qualified Syntax.Variable as Variable
import Prelude hiding (head, tail)

data Pattern position
  = -- |
    -- > x
    Variable
      { startPosition :: !position,
        variable :: !(Marked.Variable position)
      }
  | -- |
    -- > x@x
    At
      { startPosition :: !position,
        variable :: !(Marked.Variable position),
        patternx :: !(Pattern position)
      }
  | -- |
    -- > C
    Constructor
      { startPosition :: !position,
        constructor :: !QualifiedConstructor,
        patterns :: !(Strict.Vector (Pattern position))
      }
  | -- |
    -- > C { x = p }
    Record
      { startPosition :: !position,
        constructor :: !QualifiedConstructor,
        fields :: !(Strict.Vector (Field position))
      }
  | -- |
    -- > 1
    Integer
      { startPosition :: !position,
        integer :: !Integer
      }
  | -- |
    -- > 1.0
    Float
      { startPosition :: !position,
        float :: !Rational
      }
  | -- |
    -- > -1
    NegativeInteger
      { startPosition :: !position,
        integer :: !Integer
      }
  | -- |
    -- > -1.0
    NegativeFloat
      { startPosition :: !position,
        float :: !Rational
      }
  | -- |
    -- > 'a'
    Character
      { startPosition :: !position,
        character :: !Char
      }
  | -- |
    -- > "a"
    String
      { startPosition :: !position,
        string :: !StringLiteral
      }
  | -- |
    -- > _
    Wildcard
      {startPosition :: !position}
  | -- |
    -- > ()
    Unit
      {startPosition :: !position}
  | -- |
    -- > (x, x)
    Tuple
      { startPosition :: !position,
        elements :: !(Strict.Vector2 (Pattern position))
      }
  | -- |
    -- > []
    List
      { startPosition :: !position,
        items :: !(Strict.Vector (Pattern position))
      }
  | -- |
    -- > ~x
    Irrefutable
      { startPosition :: !position,
        patternx :: !(Pattern position)
      }
  | -- |
    -- > (:) x x
    Cons
      { startPosition :: !position,
        head :: !(Pattern position),
        tail :: !(Pattern position)
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

instance TermBindingVariables Pattern where
  termBindingVariables = \case
    Variable {variable} -> [variable]
    At {variable, patternx} -> variable : termBindingVariables patternx
    Constructor {patterns} -> foldMap termBindingVariables patterns
    Record {fields} -> foldMap termBindingVariables fields
    Integer {} -> []
    Float {} -> []
    NegativeInteger {} -> []
    NegativeFloat {} -> []
    Character {} -> []
    String {} -> []
    Wildcard {} -> []
    Unit {} -> []
    Tuple {elements} -> foldMap termBindingVariables elements
    List {items} -> foldMap termBindingVariables items
    Irrefutable {patternx} -> termBindingVariables patternx
    Cons {head, tail} -> termBindingVariables head ++ termBindingVariables tail
    Infix {left, right} -> termBindingVariables left ++ termBindingVariables right
    InfixCons {left, right} -> termBindingVariables left ++ termBindingVariables right

data Field position
  = -- |
    -- > C { x = x }
    -- >     ^^^^^
    Field
      { index :: !(Marked.QualifiedVariable position),
        field :: !(Pattern position)
      }
  | -- |
    -- > C { x }
    -- >     ^
    Pun
      { index :: !(Marked.QualifiedVariable position)
      }
  deriving (Show)

instance TermBindingVariables Field where
  termBindingVariables = \case
    Field {field} -> termBindingVariables field
    Pun {index = position :@ _ :- variable} -> [position :@ variable]

parse :: Parser (Pattern Position)
parse = PatternInfix.toPattern <$> PatternInfix.parse

-- lpat
parse1 :: Parser (Pattern Position)
parse1 =
  asum
    [ try $
        constructor
          <$> position
          <*> Variable.parseLiteral
          <*> (Strict.Vector.fromList <$> Applicative.some parse2),
      cons <$ try (betweenParens $ token ":") <*> position <*> parse2 <*> parse2,
      -- This is not a part of PattenInfix because the Haskell report demands
      -- that pattern negation have max precedence.
      position
        <**> ( token "-"
                 *> asum
                   [ integer <$> Parser.integer,
                     float <$> decimal
                   ]
             ),
      parse2
    ]
  where
    constructor startPosition constructor patterns =
      Constructor
        { startPosition,
          constructor,
          patterns
        }
    cons startPosition head tail =
      Cons
        { startPosition,
          head,
          tail
        }

    integer integer startPosition = Integer {startPosition, integer}

    float float startPosition = Float {startPosition, float}

-- apat
parse2 :: Parser (Pattern Position)
parse2 =
  asum
    [ Marked.parseLiteral
        <**> asum
          [ at <$> (token "@" *> parse2),
            pure variable
          ],
      tuple <$> position <*> try (betweenParens (sepByComma parse)),
      list <$> position <*> betweenBrackets (Strict.Vector.fromList <$> sepByComma parse),
      position <**> (Variable.parseLiteral <**> parseConstructor),
      integer <$> position <*> Parser.integer,
      float <$> position <*> decimal,
      character <$> position <*> char,
      string <$> position <*> text,
      wildcard <$> position <* token "_",
      irrefutable <$> position <*> (token "~" *> parse2)
    ]
  where
    variable variable@(startPosition :@ _) =
      Variable
        { startPosition,
          variable
        }
    at patternx variable@(startPosition :@ _) =
      At
        { startPosition,
          variable,
          patternx
        }

    constructor constructor startPosition =
      Constructor
        { startPosition,
          constructor,
          patterns = Strict.Vector.empty
        }

    record fields constructor startPosition =
      Record
        { startPosition,
          constructor,
          fields
        }

    tuple startPosition [] = Unit {startPosition}
    tuple _ [pattern] = pattern
    tuple startPosition (pattern : pattern' : patterns) =
      Tuple
        { startPosition,
          elements = Strict.fromList'' pattern pattern' patterns
        }

    list startPosition items = List {startPosition, items}

    integer startPosition integer = Integer {startPosition, integer}

    float startPosition float = Float {startPosition, float}
    character startPosition character = Character {startPosition, character}
    string startPosition string = String {startPosition, string}
    wildcard startPosition = Wildcard {startPosition}
    irrefutable startPosition patternx = Irrefutable {startPosition, patternx}

    parseConstructor =
      asum
        [ record <$> parseFields,
          pure constructor
        ]
    parseFields = betweenBraces (Strict.Vector.fromList <$> sepByComma parseField) where
    parseField =
      Marked.parseLiteral
        <**> asum
          [ field <$> (token "=" *> parse),
            pure pun
          ]
      where
        field field index =
          Field
            { index,
              field
            }
        pun index =
          Pun
            { index
            }
