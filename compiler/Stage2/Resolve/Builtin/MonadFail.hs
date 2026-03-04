module Stage2.Resolve.Builtin.MonadFail (fail, monadFail) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (VariableIdentifier))
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (..), fail)

monadFailName = constructorIdentifier (pack "MonadFail")

failName = VariableIdentifier $ variableIdentifier (pack "fail")

name = \case
  Method.Fail -> failName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  Prelude.pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

fail =
  ( failName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fail,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

monadFail =
  ( monadFailName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.MonadFail,
        methods,
        fields,
        constructors = Set.empty
      }
  )
