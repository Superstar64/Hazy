module Semantic.Resolve.Builtin.Fractional where

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
import Syntax.Variable (Variable (VariableIdentifier, VariableSymbol))
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
