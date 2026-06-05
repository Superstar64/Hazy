module Semantic.Resolve.Builtin.Bool where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor (false, true)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Binding.Type as Type
import Syntax.Lexer (constructorIdentifier)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Constructor (..))
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
        fielded = False,
        single = False
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
        fielded = False,
        single = False
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
