module Stage2.Resolve.Builtin (builtin) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Stage1.Position as Position
import qualified Stage1.Tree.Associativity as Associativity
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Constructor (ConstructorIdentifier), Variable (..))
import qualified Stage2.Index.Constructor as Constructor (false, true)
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Stage2.Resolve.Bindings (Bindings (..))
import Prelude hiding
  ( abs,
    enumFrom,
    enumFromThen,
    enumFromThenTo,
    enumFromTo,
    fromEnum,
    fromInteger,
    negate,
    pred,
    signum,
    succ,
    toEnum,
  )
import qualified Prelude

falseName = ConstructorIdentifier $ constructorIdentifier (pack "False")

trueName = ConstructorIdentifier $ constructorIdentifier (pack "True")

boolName = constructorIdentifier (pack "Bool")

charName = constructorIdentifier (pack "Char")

typeName = constructorIdentifier (pack "Type")

constraintName = constructorIdentifier (pack "Constraint")

smallName = constructorIdentifier (pack "Small")

largeName = constructorIdentifier (pack "Large")

universeName = constructorIdentifier (pack "Universe")

stName = constructorIdentifier (pack "ST")

runSTName = VariableIdentifier $ variableIdentifier (pack "runST")

integerName = constructorIdentifier (pack "Integer")

intName = constructorIdentifier (pack "Int")

numName = constructorIdentifier (pack "Num")

plusName = VariableSymbol $ variableSymbol (pack "+")

minusName = VariableSymbol $ variableSymbol (pack "-")

multiplyName = VariableSymbol $ variableSymbol (pack "*")

negateName = VariableIdentifier $ variableIdentifier (pack "negate")

absName = VariableIdentifier $ variableIdentifier (pack "abs")

signumName = VariableIdentifier $ variableIdentifier (pack "signum")

fromIntegerName = VariableIdentifier $ variableIdentifier (pack "fromInteger")

enumName = constructorIdentifier (pack "Enum")

succName = VariableIdentifier $ variableIdentifier (pack "succ")

predName = VariableIdentifier $ variableIdentifier (pack "pred")

toEnumName = VariableIdentifier $ variableIdentifier (pack "toEnum")

fromEnumName = VariableIdentifier $ variableIdentifier (pack "fromEnum")

enumFromName = VariableIdentifier $ variableIdentifier (pack "enumFrom")

enumFromThenName = VariableIdentifier $ variableIdentifier (pack "enumFromThen")

enumFromToName = VariableIdentifier $ variableIdentifier (pack "enumFromTo")

enumFromThenToName = VariableIdentifier $ variableIdentifier (pack "enumFromThenTo")

class (Enum enum, Bounded enum) => MethodNames enum where
  name :: enum -> Variable

instance MethodNames Method.Num where
  name = \case
    Method.Plus -> plusName
    Method.Minus -> minusName
    Method.Multiply -> multiplyName
    Method.Negate -> negateName
    Method.Abs -> absName
    Method.Signum -> signumName
    Method.FromInteger -> fromIntegerName

instance MethodNames Method.Enum where
  name = \case
    Method.Succ -> succName
    Method.Pred -> predName
    Method.ToEnum -> toEnumName
    Method.FromEnum -> fromEnumName
    Method.EnumFrom -> enumFromName
    Method.EnumFromThen -> enumFromThenName
    Method.EnumFromTo -> enumFromToName
    Method.EnumFromThenTo -> enumFromThenToName

data Proxy e = Proxy

methods :: forall e. (MethodNames e) => Proxy e -> Map Variable Int
methods Proxy = Map.fromList $ do
  enum <- [minBound .. maxBound] :: [e]
  pure (name enum, Prelude.fromEnum enum)

fields :: forall e. (MethodNames e) => Proxy e -> Set Variable
fields proxy = Map.keysSet (methods proxy)

false =
  ( falseName,
    Constructor.Binding
      { Constructor.position = Position.internal,
        Constructor.index = Constructor.false,
        Constructor.fixity = Fixity Associativity.Left 9,
        Constructor.fields = Map.empty,
        Constructor.selections = Strict.Vector.empty,
        Constructor.unordered = False,
        Constructor.fielded = False
      }
  )

true =
  ( trueName,
    Constructor.Binding
      { Constructor.position = Position.internal,
        Constructor.index = Constructor.true,
        Constructor.fixity = Fixity Associativity.Left 9,
        Constructor.fields = Map.empty,
        Constructor.selections = Strict.Vector.empty,
        Constructor.unordered = False,
        Constructor.fielded = False
      }
  )

bool =
  ( boolName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index $ Type2.Bool,
        Type.constructors = Set.fromList [falseName, trueName],
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

char =
  ( charName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index $ Type2.Char,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

typex =
  ( typeName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Type,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

constraint =
  ( constraintName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Constraint,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

small =
  ( smallName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Small,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

large =
  ( largeName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Large,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

universe =
  ( universeName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Universe,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

st =
  ( stName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index $ Type2.ST,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

runST =
  ( runSTName,
    Term.Binding
      { Term.position = Position.internal,
        Term.fixity = Fixity Associativity.Left 9,
        Term.index = Term2.RunST,
        Term.selector = Normal
      }
  )

integer =
  ( integerName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index Type2.Integer,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

int =
  ( intName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index Type2.Int,
        Type.constructors = Set.empty,
        Type.fields = Set.empty,
        Type.methods = Map.empty
      }
  )

num =
  ( numName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index Type2.Num,
        Type.constructors = Set.empty,
        Type.fields = fields (Proxy :: Proxy Method.Num),
        Type.methods = methods (Proxy :: Proxy Method.Num)
      }
  )

plus =
  ( plusName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.plus,
        Term.fixity = Fixity Associativity.Left 6,
        Term.selector = Normal
      }
  )

minus =
  ( minusName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.minus,
        Term.fixity = Fixity Associativity.Left 6,
        Term.selector = Normal
      }
  )

multiply =
  ( multiplyName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.multiply,
        Term.fixity = Fixity Associativity.Left 7,
        Term.selector = Normal
      }
  )

negate =
  ( negateName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.negate,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

abs =
  ( absName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.abs,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

signum =
  ( signumName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.signum,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

fromInteger =
  ( fromIntegerName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.fromInteger,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

enum =
  ( enumName,
    Type.Binding
      { Type.position = Position.internal,
        Type.index = Type3.Index Type2.Enum,
        Type.constructors = Set.empty,
        Type.fields = fields (Proxy :: Proxy Method.Enum),
        Type.methods = methods (Proxy :: Proxy Method.Enum)
      }
  )

succ =
  ( succName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.succ,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

pred =
  ( predName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.pred,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

toEnum =
  ( toEnumName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.toEnum,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

fromEnum =
  ( fromEnumName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.fromEnum,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

enumFrom =
  ( enumFromName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.enumFrom,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

enumFromThen =
  ( enumFromThenName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.enumFromThen,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

enumFromTo =
  ( enumFromToName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.enumFromTo,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

enumFromThenTo =
  ( enumFromThenToName,
    Term.Binding
      { Term.position = Position.internal,
        Term.index = Term2.Method Method.enumFromThenTo,
        Term.fixity = Fixity Associativity.Left 9,
        Term.selector = Normal
      }
  )

builtin :: Bindings () scope
builtin =
  Bindings
    { terms =
        Map.fromListWith
          undefined
          [ plus,
            minus,
            multiply,
            negate,
            abs,
            signum,
            fromInteger,
            succ,
            pred,
            toEnum,
            fromEnum,
            enumFrom,
            enumFromThen,
            enumFromTo,
            enumFromThenTo,
            runST
          ],
      constructors = Map.fromListWith undefined [false, true],
      types =
        Map.fromListWith
          undefined
          [ bool,
            char,
            typex,
            constraint,
            small,
            large,
            universe,
            st,
            integer,
            int,
            num,
            enum
          ],
      stability = ()
    }
