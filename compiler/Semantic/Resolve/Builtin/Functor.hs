module Semantic.Resolve.Builtin.Functor (fmap, fconst, functor) where

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
import Prelude hiding (Either (..), fmap)

functorName = constructorIdentifier (pack "Functor")

fmapName = VariableIdentifier $ variableIdentifier (pack "fmap")

fconstName = VariableSymbol $ variableSymbol (pack "<$")

name = \case
  Method.Fmap -> fmapName
  Method.Fconst -> fconstName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

fmap =
  ( fmapName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fmap,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fconst =
  ( fconstName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fconst,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

functor =
  ( functorName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Functor,
        methods,
        fields,
        constructors = Set.empty
      }
  )
