module Stage2.Resolve.Builtin.Functor (fmap, fconst, functor) where

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
