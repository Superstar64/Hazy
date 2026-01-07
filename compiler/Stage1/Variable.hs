-- |
-- Variables
module Stage1.Variable
  ( Constructor (..),
    ConstructorIdentifier,
    ConstructorSymbol,
    FullQualifiers (..),
    toQualifiers,
    Name (..),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier (..),
    QualifiedConstructorSymbol (..),
    QualifiedName (..),
    QualifiedVariable (..),
    QualifiedVariableIdentifier (..),
    QualifiedVariableSymbol (..),
    Qualifiers (..),
    Variable (..),
    VariableIdentifier,
    VariableSymbol,
    FullyQualifiedVariable (..),
    FullyQualifiedConstructor (..),
    FullyQualifiedConstructorIdentifier (..),
    Canonical (..),
    print',
    CanonicalVariable (..),
    printLiteral',
    prelude,
    prelude',
  )
where

import Data.Text (pack)
import Data.Text.Lazy.Builder (Builder)
import qualified Error as Strings
import Stage1.Lexer
  ( ConstructorIdentifier,
    ConstructorSymbol,
    FullQualifiers (..),
    Lexer (..),
    Qualifiers (..),
    VariableIdentifier,
    VariableSymbol,
    constructorIdentifier,
    toQualifiers,
  )
import qualified Stage1.Lexer as Lexer (FullQualifiers (..), Lexeme (..), Qualifiers (..))
import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    betweenTicks,
    try,
  )
import Stage1.ParserCombinator (Bind (..), satifyBind)
import Stage1.Printer (Printed, build)
import qualified Stage1.Printer as Printer
import Prelude hiding (print)

class Canonical variable where
  print :: variable -> Printed
  parse :: Parser variable

print' :: (Canonical variable) => variable -> Builder
print' = build . print

class CanonicalVariable variable where
  parseLiteral :: Parser variable
  parseOperator :: Parser variable
  printLiteral :: variable -> Printed
  printOperator :: variable -> Printed

printLiteral' :: (CanonicalVariable variable) => variable -> Builder
printLiteral' = build . printLiteral

instance Canonical VariableIdentifier where
  parse = satifyBind Strings.variableIdentifier $ \case
    Lex _ (Local Lexer.:-. name) lexer _ -> Parse name lexer
    _ -> Fail
  print variable = Printer.token' (Lexer.Local Lexer.:-. variable)

instance Canonical VariableSymbol where
  parse = satifyBind Strings.variableSymbol $ \case
    Lex _ (Local Lexer.:-: name) lexer _ -> Parse name lexer
    _ -> Fail
  print name = Printer.token' (Local Lexer.:-: name)

instance Canonical ConstructorIdentifier where
  print name = Printer.token' (Local Lexer.:=. name)
  parse = satifyBind Strings.constructorIdentifier $ \case
    Lex _ (Local Lexer.:=. name) lexer _ -> Parse name lexer
    _ -> Fail

instance Canonical ConstructorSymbol where
  parse = satifyBind Strings.constructorSymbol $ \case
    Lex _ (Local Lexer.:=: name) lexer _ -> Parse name lexer
    _ -> Fail
  print name = Printer.token' (Local Lexer.:=: name)

instance Canonical FullQualifiers where
  parse = satifyBind Strings.moduleIdentifier $ \case
    Lex _ (qualifiers Lexer.:=. name) lexer _ -> Parse (qualifiers :.. name) lexer
    _ -> Fail
  print (qualifiers Lexer.:.. name) = Printer.token' (qualifiers Lexer.:=. name)

class (Canonical variable) => CanonicalWrapped variable where
  parseWrapped :: Parser variable
  printWrapped :: variable -> Printed

instance CanonicalWrapped VariableIdentifier where
  parseWrapped = try $ betweenTicks parse

instance CanonicalWrapped VariableSymbol where
  parseWrapped = try $ betweenParens parse
  printWrapped name =
    mconcat
      [ Printer.token "(",
        print name,
        Printer.token ")"
      ]

instance CanonicalWrapped ConstructorIdentifier where
  parseWrapped = try $ betweenTicks parse

instance CanonicalWrapped ConstructorSymbol where
  parseWrapped = try $ betweenParens parse
  printWrapped name =
    mconcat
      [ Printer.token "(",
        print name,
        Printer.token ")"
      ]

data QualifiedVariableIdentifier = !Qualifiers :-. !VariableIdentifier
  deriving (Show, Eq, Ord)

infixl 5 :-.

instance Canonical QualifiedVariableIdentifier where
  parse = satifyBind Strings.qualifiedVariableIdentifier $ \case
    Lex _ (qualifiers Lexer.:-. name) lexer _ -> Parse (qualifiers :-. name) lexer
    _ -> Fail
  print (qualifiers :-. name) = Printer.token' (qualifiers Lexer.:-. name)

instance CanonicalWrapped QualifiedVariableIdentifier where
  parseWrapped = try $ betweenTicks parse

data QualifiedVariableSymbol = !Qualifiers :-: !VariableSymbol
  deriving (Show, Eq, Ord)

infixl 5 :-:

instance Canonical QualifiedVariableSymbol where
  parse = satifyBind Strings.qualifiedVariableSymbol $ \case
    Lex _ (qualifiers Lexer.:-: name) lexer _ -> Parse (qualifiers :-: name) lexer
    _ -> Fail
  print (qualifiers :-: name) = Printer.token' (qualifiers Lexer.:-: name)

instance CanonicalWrapped QualifiedVariableSymbol where
  parseWrapped = try $ betweenParens parse
  printWrapped name =
    mconcat
      [ Printer.token "(",
        print name,
        Printer.token ")"
      ]

data QualifiedConstructorIdentifier = !Qualifiers :=. !ConstructorIdentifier
  deriving (Show, Eq, Ord)

infixl 5 :=.

instance Canonical QualifiedConstructorIdentifier where
  parse = satifyBind Strings.qualifiedConstructorIdentifier $ \case
    Lex _ (qualifiers Lexer.:=. name) lexer _ -> Parse (qualifiers :=. name) lexer
    _ -> Fail
  print (root :=. name) = Printer.token' (root Lexer.:=. name)

instance CanonicalWrapped QualifiedConstructorIdentifier where
  parseWrapped = try $ betweenTicks parse

data QualifiedConstructorSymbol = !Qualifiers :=: !ConstructorSymbol
  deriving (Show, Eq, Ord)

infixl 5 :=:

instance Canonical QualifiedConstructorSymbol where
  parse = satifyBind Strings.qualifiedConstructorSymbol $ \case
    Lex _ (qualifiers Lexer.:=: name) lexer _ -> Parse (qualifiers :=: name) lexer
    _ -> Fail

instance CanonicalWrapped QualifiedConstructorSymbol where
  parseWrapped = try $ betweenParens parse

data Variable
  = VariableIdentifier !VariableIdentifier
  | VariableSymbol !VariableSymbol
  deriving (Show, Eq, Ord)

instance CanonicalVariable Variable where
  parseLiteral =
    asum
      [ VariableIdentifier <$> parse,
        VariableSymbol <$> parseWrapped
      ]

  parseOperator =
    asum
      [ VariableIdentifier <$> parseWrapped,
        VariableSymbol <$> parse
      ]

  printLiteral (VariableIdentifier name) = print name
  printLiteral (VariableSymbol name) = printWrapped name

data Constructor
  = ConstructorIdentifier !ConstructorIdentifier
  | ConstructorSymbol !ConstructorSymbol
  deriving (Show, Eq, Ord)

instance CanonicalVariable Constructor where
  parseLiteral =
    asum
      [ ConstructorIdentifier <$> parse,
        ConstructorSymbol <$> parseWrapped
      ]

  parseOperator =
    asum
      [ ConstructorIdentifier <$> parseWrapped,
        ConstructorSymbol <$> parse
      ]
  printLiteral (ConstructorIdentifier name) = print name
  printLiteral (ConstructorSymbol name) = printWrapped name

data Name
  = Variable !Variable
  | Constructor !Constructor
  deriving (Show, Eq, Ord)

instance CanonicalVariable Name where
  parseLiteral =
    asum
      [ Variable <$> parseLiteral,
        Constructor <$> parseLiteral
      ]

  parseOperator =
    asum
      [ Variable <$> parseOperator,
        Constructor <$> parseOperator
      ]

data QualifiedVariable
  = !Qualifiers :- !Variable
  deriving (Show, Eq, Ord)

infixl 5 :-

shiftIdentifierVariable (qualifiers :-. name) = qualifiers :- VariableIdentifier name

shiftSymbolVariable (qualifiers :-: name) = qualifiers :- VariableSymbol name

instance CanonicalVariable QualifiedVariable where
  parseLiteral =
    asum
      [ shiftIdentifierVariable <$> parse,
        shiftSymbolVariable <$> parseWrapped
      ]

  parseOperator =
    asum
      [ shiftSymbolVariable <$> parse,
        shiftIdentifierVariable <$> parseWrapped
      ]
  printLiteral (qualifiers :- VariableIdentifier name) = print (qualifiers :-. name)
  printLiteral (qualifiers :- VariableSymbol name) = printWrapped (qualifiers :-: name)

data QualifiedConstructor
  = !Qualifiers := !Constructor
  deriving (Show, Eq, Ord)

infixl 5 :=

shiftIdentifierConstructor (qualifiers :=. name) = qualifiers := ConstructorIdentifier name

shiftSymbolConstructor (qualifiers :=: name) = qualifiers := ConstructorSymbol name

instance CanonicalVariable QualifiedConstructor where
  parseLiteral =
    asum
      [ shiftIdentifierConstructor <$> parse,
        shiftSymbolConstructor <$> parseWrapped
      ]

  parseOperator =
    asum
      [ shiftSymbolConstructor <$> parse,
        shiftIdentifierConstructor <$> parseWrapped
      ]
  printLiteral (qualifiers := ConstructorIdentifier name) = print (qualifiers :=. name)
  printLiteral (qualifiers := ConstructorSymbol name) = printWrapped (qualifiers :=: name)

data QualifiedName
  = QualifiedVariable !QualifiedVariable
  | QualifiedConstructor !QualifiedConstructor
  deriving (Show, Eq, Ord)

instance CanonicalVariable QualifiedName where
  parseLiteral =
    asum
      [ QualifiedVariable <$> parseLiteral,
        QualifiedConstructor <$> parseLiteral
      ]

  parseOperator =
    asum
      [ QualifiedVariable <$> parseOperator,
        QualifiedConstructor <$> parseOperator
      ]

data FullyQualifiedVariable = !FullQualifiers :.- !Variable
  deriving (Eq, Ord, Show)

data FullyQualifiedConstructor = !FullQualifiers :.= !Constructor
  deriving (Eq, Ord, Show)

data FullyQualifiedConstructorIdentifier = !FullQualifiers :.=. !ConstructorIdentifier
  deriving (Eq, Ord, Show)

infixl 5 :.-, :.=, :.=.

prelude :: FullQualifiers
prelude = Local :.. constructorIdentifier (pack "Prelude")

prelude' :: Qualifiers
prelude' | _ :.. prelude <- prelude = Local :. prelude
