module Semantic.Resolve.Builtin.Ratio where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor (makeRatio)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Binding.Type as Type
import Syntax.Lexer (constructorIdentifier, constructorSymbol)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Constructor (..))
import Prelude hiding (Either (..))

ratioName = constructorIdentifier $ pack "Ratio"

makeName = ConstructorSymbol $ constructorSymbol $ pack ":%"

make =
  ( makeName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.makeRatio,
        fixity = Fixity {associativity = Left, precedence = 7},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False,
        single = True
      }
  )

ratio =
  ( ratioName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Ratio,
        methods = Map.empty,
        constructors = Set.singleton makeName,
        fields = Set.empty
      }
  )
