module Stage2.Resolve.Builtin.Num
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
import Stage1.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (..))
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
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
