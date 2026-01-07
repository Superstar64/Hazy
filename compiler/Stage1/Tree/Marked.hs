-- |
-- A wrapper of a variable with position data
module Stage1.Tree.Marked where

import Stage1.Parser (Parser, position)
import Stage1.Position (Position)
import Stage1.Printer (Printed)
import qualified Stage1.Variable as Variable
import Prelude hiding (print)

infix 4 :@

-- |
-- A variable marked with a position
data Marked variable position = !position :@ !variable
  deriving (Show)

parse :: (Variable.Canonical variable) => Parser (Marked variable Position)
parse = (:@) <$> position <*> Variable.parse

print :: (Variable.Canonical variable) => Marked variable unit -> Printed
print (_ :@ node) = Variable.print node

parseLiteral :: (Variable.CanonicalVariable variable) => Parser (Marked variable Position)
parseLiteral = (:@) <$> position <*> Variable.parseLiteral

printLiteral :: (Variable.CanonicalVariable variable) => Marked variable unit -> Printed
printLiteral (_ :@ node) = Variable.printLiteral node

parseOperator :: (Variable.CanonicalVariable variable) => Parser (Marked variable Position)
parseOperator = (:@) <$> position <*> Variable.parseOperator

printOperator :: (Variable.CanonicalVariable variable) => Marked variable unit -> Printed
printOperator (_ :@ node) = Variable.printOperator node

type Constructor = Marked Variable.Constructor

type ConstructorIdentifier = Marked Variable.ConstructorIdentifier

type ConstructorSymbol = Marked Variable.ConstructorSymbol

type FullQualifiers = Marked Variable.FullQualifiers

type Name = Marked Variable.Name

type QualifiedConstructor = Marked Variable.QualifiedConstructor

type QualifiedConstructorIdentifier = Marked Variable.QualifiedConstructorIdentifier

type QualifiedConstructorSymbol = Marked Variable.QualifiedConstructorSymbol

type QualifiedName = Marked Variable.QualifiedName

type QualifiedVariable = Marked Variable.QualifiedVariable

type QualifiedVariableIdentifier = Marked Variable.QualifiedVariableIdentifier

type QualifiedVariableSymbol = Marked Variable.QualifiedVariableSymbol

type Qualifiers = Marked Variable.Qualifiers

type Variable = Marked Variable.Variable

type VariableIdentifier = Marked Variable.VariableIdentifier

type VariableSymbol = Marked Variable.VariableSymbol

type FullyQualifiedVariable = Marked Variable.FullyQualifiedVariable

type FullyQualifiedConstructor = Marked Variable.FullyQualifiedConstructor

type FullyQualifiedConstructorIdentifier = Marked Variable.FullyQualifiedConstructorIdentifier
