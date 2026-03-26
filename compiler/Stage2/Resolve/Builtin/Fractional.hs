module Stage2.Resolve.Builtin.Fractional where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (VariableIdentifier, VariableSymbol))
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (..))

fractionalName = constructorIdentifier $ pack "Fractional"

divideName = VariableSymbol $ variableSymbol $ pack "/"

recipName = VariableIdentifier $ variableIdentifier $ pack "recip"

fromRationalName = VariableIdentifier $ variableIdentifier $ pack "fromRational"

name = \case
  Method.Divide -> divideName
  Method.Recip -> recipName
  Method.FromRational -> fromRationalName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

divide =
  ( divideName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.divide,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

recip =
  ( recipName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.recip,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fromRational =
  ( fromRationalName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fromRational,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fractional =
  ( fractionalName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Fractional,
        methods,
        fields,
        constructors = Set.empty
      }
  )
