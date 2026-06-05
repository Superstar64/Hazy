module Semantic.Resolve.Builtin.MonadFail (fail, monadFail) where

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
import Syntax.Lexer (constructorIdentifier, variableIdentifier)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Variable (VariableIdentifier))
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
