module Stage2.Resolve.Builtin.Monad (bind, thenx, return, monad) where

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
