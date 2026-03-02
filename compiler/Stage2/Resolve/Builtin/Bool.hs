module Stage2.Resolve.Builtin.Bool where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Constructor (..))
import qualified Stage2.Index.Constructor as Constructor (false, true)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (Left, Right))

boolName = constructorIdentifier (pack "Bool")

falseName = ConstructorIdentifier $ constructorIdentifier (pack "False")

trueName = ConstructorIdentifier $ constructorIdentifier (pack "True")

false =
  ( falseName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.false,
        fixity = Fixity {associativity = Left, precedence = 9},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False
      }
  )

true =
  ( trueName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.true,
        fixity = Fixity {associativity = Left, precedence = 9},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False
      }
  )

bool =
  ( boolName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Bool,
        constructors = Set.fromList [falseName, trueName],
        fields = Set.empty,
        methods = Map.empty
      }
  )
