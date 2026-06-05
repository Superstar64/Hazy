module Semantic.Resolve.Builtin.Eq (equal, notEqual, eq) where

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
import Syntax.Lexer (constructorIdentifier, variableSymbol)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Variable (..))
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
