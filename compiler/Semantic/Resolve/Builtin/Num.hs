module Semantic.Resolve.Builtin.Num
  ( plus,
    minus,
    multiply,
    negate,
    abs,
    signum,
    fromInteger,
    num,
  )
where

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
import Prelude hiding (Either (..), Num (..))

numName = constructorIdentifier (pack "Num")

plusName = VariableSymbol $ variableSymbol (pack "+")

minusName = VariableSymbol $ variableSymbol (pack "-")

multiplyName = VariableSymbol $ variableSymbol (pack "*")

negateName = VariableIdentifier $ variableIdentifier (pack "negate")

absName = VariableIdentifier $ variableIdentifier (pack "abs")

signumName = VariableIdentifier $ variableIdentifier (pack "signum")

fromIntegerName = VariableIdentifier $ variableIdentifier (pack "fromInteger")

name = \case
  Method.Plus -> plusName
  Method.Minus -> minusName
  Method.Multiply -> multiplyName
  Method.Negate -> negateName
  Method.Abs -> absName
  Method.Signum -> signumName
  Method.FromInteger -> fromIntegerName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

plus =
  ( plusName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.plus,
        fixity = Fixity {associativity = Left, precedence = 6},
        selector = Normal
      }
  )

minus =
  ( minusName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.minus,
        fixity = Fixity {associativity = Left, precedence = 6},
        selector = Normal
      }
  )

multiply =
  ( multiplyName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.multiply,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

negate =
  ( negateName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.negate,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

abs =
  ( absName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.abs,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

signum =
  ( signumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.signum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fromInteger =
  ( fromIntegerName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fromInteger,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

num =
  ( numName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Num,
        constructors = Set.empty,
        fields,
        methods
      }
  )
