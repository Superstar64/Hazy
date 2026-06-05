module Semantic.Resolve.Builtin.Ord where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Binding.Term (Selector (Normal))
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Syntax.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Variable (..))
import Prelude hiding (Either (..))

ordName = constructorIdentifier (pack "Ord")

compareName = VariableIdentifier $ variableIdentifier (pack "compare")

lessThenName = VariableSymbol $ variableSymbol (pack "<")

lessThenEqualName = VariableSymbol $ variableSymbol (pack "<=")

greaterThenName = VariableSymbol $ variableSymbol (pack ">")

greaterThenEqualName = VariableSymbol $ variableSymbol (pack ">=")

maxName = VariableIdentifier $ variableIdentifier (pack "max")

minName = VariableIdentifier $ variableIdentifier (pack "min")

name = \case
  Method.Compare -> compareName
  Method.LessThen -> lessThenName
  Method.LessThenEqual -> lessThenEqualName
  Method.GreaterThen -> greaterThenName
  Method.GreaterThenEqual -> greaterThenEqualName
  Method.Max -> maxName
  Method.Min -> minName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

compare =
  ( compareName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.compare,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

lessThen =
  ( lessThenName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.lessThen,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
      }
  )

lessThenEqual =
  ( lessThenEqualName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.lessThenEqual,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

greaterThen =
  ( greaterThenName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.greaterThen,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
      }
  )

greaterThenEqual =
  ( greaterThenEqualName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.greaterThenEqual,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

max =
  ( maxName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.max,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

min =
  ( minName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.min,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

ord =
  ( ordName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Ord,
        methods,
        fields,
        constructors = Set.empty
      }
  )
