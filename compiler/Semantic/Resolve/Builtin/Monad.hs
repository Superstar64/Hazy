module Semantic.Resolve.Builtin.Monad (bind, thenx, return, monad) where

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
import Prelude hiding (Either (..), return)

monadName = constructorIdentifier (pack "Monad")

bindName = VariableSymbol $ variableSymbol (pack ">>=")

thenName = VariableSymbol $ variableSymbol (pack ">>")

returnName = VariableIdentifier $ variableIdentifier (pack "return")

name = \case
  Method.Bind -> bindName
  Method.Then -> thenName
  Method.Return -> returnName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  Prelude.pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

bind =
  ( bindName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.bind,
        fixity = Fixity {associativity = Left, precedence = 1},
        selector = Normal
      }
  )

thenx =
  ( thenName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.thenx,
        fixity = Fixity {associativity = Left, precedence = 1},
        selector = Normal
      }
  )

return =
  ( returnName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.return,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

monad =
  ( monadName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Monad,
        methods,
        fields,
        constructors = Set.empty
      }
  )
