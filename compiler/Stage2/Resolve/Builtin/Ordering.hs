module Stage2.Resolve.Builtin.Ordering where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Constructor (..))
import qualified Stage2.Index.Constructor as Constructor (eq, gt, lt)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (Left, Right))

orderingName = constructorIdentifier $ pack "Ordering"

ltName = ConstructorIdentifier $ constructorIdentifier $ pack "LT"

eqName = ConstructorIdentifier $ constructorIdentifier $ pack "EQ"

gtName = ConstructorIdentifier $ constructorIdentifier $ pack "GT"

lt =
  ( ltName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.lt,
        fixity = Fixity {associativity = Left, precedence = 9},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False,
        single = False
      }
  )

eq =
  ( eqName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.eq,
        fixity = Fixity {associativity = Left, precedence = 9},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False,
        single = False
      }
  )

gt =
  ( gtName,
    Constructor.Binding
      { position = Position.internal,
        index = Constructor.gt,
        fixity = Fixity {associativity = Left, precedence = 9},
        fields = Map.empty,
        selections = Strict.Vector.empty,
        unordered = False,
        fielded = False,
        single = False
      }
  )

ordering =
  ( orderingName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Ordering,
        methods = Map.empty,
        fields = Set.empty,
        constructors = Set.fromList [ltName, eqName, gtName]
      }
  )
