module Stage2.Resolve.Builtin.Eq (equal, notEqual, eq) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableSymbol)
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
import Prelude hiding (Either (..))

eqName = constructorIdentifier (pack "Eq")

equalName = VariableSymbol $ variableSymbol (pack "==")

notEqualName = VariableSymbol $ variableSymbol (pack "/=")

name = \case
  Method.Equal -> equalName
  Method.NotEqual -> notEqualName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

equal =
  ( equalName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.equal,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
      }
  )

notEqual =
  ( notEqualName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.notEqual,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
      }
  )

eq =
  ( eqName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Eq,
        constructors = Set.empty,
        fields,
        methods
      }
  )
