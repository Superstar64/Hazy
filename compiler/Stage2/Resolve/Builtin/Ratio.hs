module Stage2.Resolve.Builtin.Ratio where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier, constructorSymbol)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Constructor (..))
import qualified Stage2.Index.Constructor as Constructor (makeRatio)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Type as Type
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
