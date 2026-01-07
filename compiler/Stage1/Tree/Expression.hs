{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for expressions
module Stage1.Tree.Expression where

import qualified Data.Strict.Vector1 as Strict (Vector1, fromNonEmpty)
import qualified Data.Strict.Vector2 as Strict (Vector2, fromList'')
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBraces,
    betweenBrackets,
    betweenParens,
    char,
    decimal,
    many,
    optional,
    position,
    sepByComma,
    sepEndBy1Semicolon,
    some,
    text,
    token,
    try,
    (<**>),
  )
import qualified Stage1.Parser as Parser
import Stage1.Position (Position)
import Stage1.Tree.Alternative (Alternative)
import qualified Stage1.Tree.Alternative as Alternative
import Stage1.Tree.Declarations (Declarations)
import qualified Stage1.Tree.Declarations as Declarations
import Stage1.Tree.ExpressionField (Field)
import qualified Stage1.Tree.ExpressionField as ExpressionField
import {-# SOURCE #-} Stage1.Tree.ExpressionInfix (Infix)
import {-# SOURCE #-} qualified Stage1.Tree.ExpressionInfix as ExpressionInfix
import Stage1.Tree.Marked (Marked ((:@)))
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Pattern (Pattern)
import qualified Stage1.Tree.Pattern as Pattern
import Stage1.Tree.RightHandSide (RightHandSide)
import qualified Stage1.Tree.RightHandSide as RightHandSide
import Stage1.Tree.Scheme (Scheme)
import qualified Stage1.Tree.Scheme as Scheme
import Stage1.Tree.Statements (Statements)
import qualified Stage1.Tree.Statements as Statements

data Expression position
  = -- |
    -- > x
    Variable
      { variable :: !(Marked.QualifiedVariable position)
      }
  | -- |
    -- > C
    Constructor
      { constructor :: !(Marked.QualifiedConstructor position)
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
    -- > 'a'
    Character
      { startPosition :: !position,
        character :: !Char
      }
  | -- |
    -- > "a"
    String
      { startPosition :: !position,
        string :: !Text
      }
  | -- |
    -- > (,,)
    Tupling
      { startPosition :: !position,
        count :: !Int
      }
  | -- |
    -- > ()
    Unit
      { startPosition :: !position
      }
  | -- |
    -- > (e, e)
    Tuple
      { startPosition :: !position,
        elements :: !(Strict.Vector2 (Expression position))
      }
  | -- |
    -- > [e, e]
    List
      { startPosition :: !position,
        items :: !(Strict.Vector (Expression position))
      }
  | -- |
    -- > [e ..]
    SequenceFrom
      { startPosition :: !position,
        from :: !(Expression position)
      }
  | -- |
    -- > [e, e ..]
    SequenceFromThen
      { startPosition :: !position,
        from :: !(Expression position),
        thenx :: !(Expression position)
      }
  | -- |
    -- > [e .. e]
    SequenceFromTo
      { startPosition :: !position,
        from :: !(Expression position),
        to :: !(Expression position)
      }
  | -- |
    -- > [e, e .. e]
    SequenceFromThenTo
      { startPosition :: !position,
        from :: !(Expression position),
        thenx :: !(Expression position),
        to :: !(Expression position)
      }
  | -- |
    -- > [e | x <- e]
    Comprehension
      { startPosition :: !position,
        statements :: !(Statements position)
      }
  | -- |
    -- > C { x = e }
    Record
      { startPosition :: !position,
        constructor :: !(Marked.QualifiedConstructor position),
        fields :: !(Strict.Vector (Field position))
      }
  | -- |
    -- > e { x = e }
    Update
      { base :: !(Expression position),
        updatePosition :: !position,
        updates :: !(Strict.Vector1 (Field position))
      }
  | -- |
    -- > e e
    Call
      { function :: !(Expression position),
        argument :: !(Expression position)
      }
  | -- |
    -- > let x = e in e
    Let
      { declarations :: !(Declarations position),
        body :: !(Expression position)
      }
  | -- |
    -- > if e then e else e
    If
      { condition :: !(Expression position),
        thenx :: !(Expression position),
        elsex :: !(Expression position)
      }
  | -- |
    -- > if { | x <- e -> e }
    MultiwayIf
      {branches :: !(Strict.Vector1 (RightHandSide position))}
  | -- |
    -- > case e of { x -> e }
    Case
      { startPosition :: !position,
        scrutinee :: !(Expression position),
        cases :: !(Strict.Vector (Alternative position))
      }
  | -- |
    -- > do { x <- e; e }
    Do
      { startPosition :: !position,
        statements :: !(Statements position)
      }
  | -- |
    -- > \x -> e
    Lambda
      { startPosition :: !position,
        parameters :: !(Strict.Vector1 (Pattern position)),
        body :: !(Expression position)
      }
  | -- |
    -- > \case { x -> e }
    LambdaCase
      { startPosition :: !position,
        cases :: !(Strict.Vector (Alternative position))
      }
  | -- |
    -- > (:)
    Cons
      { startPosition :: !position
      }
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
    -- > (e +)
    LeftSection
      { leftSection :: !(Infix position),
        operator :: !(Marked.QualifiedName position)
      }
  | -- |
    -- > (e :)
    LeftSectionCons
      { leftSection :: !(Infix position),
        operatorPosition :: !position
      }
  | -- |
    -- > (+ e)
    RightSection
      { operator :: !(Marked.QualifiedName position),
        rightSection :: !(Infix position)
      }
  | -- |
    -- > (: e)
    RightSectionCons
      { operatorPosition :: !position,
        rightSection :: !(Infix position)
      }
  | -- |
    -- > e :: t
    Annotation
      { expression :: !(Expression position),
        operatorPosition :: !position,
        annotation :: !(Scheme position)
      }
  deriving (Show)

-- exp
parse :: Parser (Expression Position)
parse =
  parse1
    <**> asum
      [ annotation <$> position <*> (token "::" *> Scheme.parse),
        pure id
      ]
  where
    annotation operatorPosition annotation expression =
      Annotation {expression, operatorPosition, annotation}

-- infixexp
parse1 :: Parser (Expression Position)
parse1 = ExpressionInfix.toExpression <$> ExpressionInfix.parse

-- lexp
parse2 :: Parser (Expression Position)
parse2 =
  asum
    [ casex <$> position <*> (token "case" *> parse) <*> (token "of" *> Alternative.parseMany),
      dox <$> position <*> (token "do" *> betweenBraces Statements.parseDo),
      letx <$> (token "let" *> Declarations.parse) <*> (token "in" *> parse),
      token "if" *> parseIf,
      (position <* token "\\") <**> parseLambda,
      foldl1 call <$> some parse3
    ]
  where
    casex startPosition scrutinee cases = Case {startPosition, scrutinee, cases}
    dox startPosition statements = Do {startPosition, statements}
    ifx condition thenx elsex = If {condition, thenx, elsex}
    multiwayIf branches = MultiwayIf {branches}
    letx declarations body = Let {declarations, body}
    lambda parameters body startPosition = Lambda {startPosition, parameters, body}
    lambdaCase cases startPosition = LambdaCase {startPosition, cases}
    call function argument = Call {function, argument}
    parseIf =
      asum
        [ ifx
            <$> parse
            <* optional (token ";")
            <*> (token "then" *> parse <* optional (token ";"))
            <*> (token "else" *> parse),
          multiwayIf . Strict.fromNonEmpty <$> betweenBraces (sepEndBy1Semicolon (RightHandSide.parse (token "->")))
        ]
    parseLambda =
      asum
        [ (lambda . Strict.fromNonEmpty <$> some Pattern.parse2) <*> (token "->" *> parse),
          lambdaCase <$> (token "case" *> Alternative.parseMany)
        ]

-- aexp
parse3 :: Parser (Expression Position)
parse3 =
  asum
    [ foldl update
        <$> try (record <$> Marked.parseLiteral <*> ExpressionField.parseMany)
        <*> many ((,) <$> position <*> ExpressionField.parseSome),
      foldl update <$> parse4 <*> many ((,) <$> position <*> ExpressionField.parseSome)
    ]
  where
    record constructor@(startPosition :@ _) fields = Record {startPosition, constructor, fields}
    update base (updatePosition, updates) =
      Update
        { base,
          updatePosition,
          updates
        }

-- aexp'
parse4 :: Parser (Expression Position)
parse4 =
  asum
    [ variable <$> Marked.parseLiteral,
      constructor <$> Marked.parseLiteral,
      try (betweenParens ExpressionInfix.parseLeftSection),
      cons <$> position <* try (betweenParens $ token ":"),
      betweenParens parseParen,
      betweenBrackets (position <**> parse5),
      integer <$> position <*> Parser.integer,
      float <$> position <*> decimal,
      character <$> position <*> char,
      string <$> position <*> text
    ]
  where
    variable variable = Variable {variable}
    constructor constructor = Constructor {constructor}
    cons startPosition = Cons {startPosition}
    integer startPosition integer = Integer {startPosition, integer}
    float startPosition float = Float {startPosition, float}
    character startPosition character = Character {startPosition, character}
    string startPosition string = String {startPosition, string}
    tupling startPosition count = Tupling {startPosition, count}
    rightSection operator rightSection =
      RightSection
        { operator,
          rightSection
        }
    rightSectionCons operatorPosition rightSection =
      RightSectionCons
        { operatorPosition,
          rightSection
        }
    tuple startPosition [] = Unit {startPosition}
    tuple _ [e] = e
    tuple startPosition (e1 : e2 : es) =
      Tuple
        { startPosition,
          elements = Strict.fromList'' e1 e2 es
        }
    parseParen =
      asum
        [ parseRightSection,
          parseRightSectionCons,
          tupling <$> position <*> (length <$> some (token ",")),
          tuple <$> position <*> sepByComma parse
        ]
      where
        parseRightSection = rightSection <$> Marked.parseOperator <*> ExpressionInfix.parse
        parseRightSectionCons = rightSectionCons <$ token ":" <*> position <*> ExpressionInfix.parse

-- brackets
parse5 :: Parser (Position -> Expression Position)
parse5 =
  asum
    [ parse <**> parse6,
      pure empty
    ]
  where
    empty startPosition = List {startPosition, items = Strict.Vector.empty}

-- brackets2
parse6 :: Parser (Expression Position -> Position -> Expression Position)
parse6 =
  asum
    [ (token "," *> parse) <**> parse7,
      comprehension <$> (token "|" *> Statements.parseComprehension),
      token ".." *> asum [sequenceFromTo <$> parse, pure sequenceFrom],
      pure list
    ]
  where
    comprehension statements base startPosition =
      Comprehension
        { startPosition,
          statements = statements base
        }
    list item startPosition =
      List
        { startPosition,
          items = Strict.Vector.singleton item
        }
    sequenceFrom from startPosition = SequenceFrom {startPosition, from}
    sequenceFromTo to from startPosition = SequenceFromTo {startPosition, from, to}

-- brackets3
parse7 :: Parser (Expression Position -> Expression Position -> Position -> Expression Position)
parse7 =
  asum
    [ token ".." *> asum [sequenceFromThenTo <$> parse, pure sequenceFromTo],
      list <$> many (token "," *> parse)
    ]
  where
    sequenceFromTo to from startPosition = SequenceFromTo {startPosition, from, to}
    sequenceFromThenTo to thenx from startPosition = SequenceFromThenTo {startPosition, from, thenx, to}
    list es e2 e1 startPosition =
      List
        { startPosition,
          items = Strict.Vector.fromList (e1 : e2 : es)
        }
