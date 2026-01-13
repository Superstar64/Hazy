module Stage2.Resolve.Builtin (builtin) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
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
  ( Either (Left, Right),
    abs,
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

eqName = constructorIdentifier (pack "Eq")

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

equalName = VariableSymbol $ variableSymbol (pack "==")

notEqualName = VariableSymbol $ variableSymbol (pack "/=")

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

instance MethodNames Method.Eq where
  name = \case
    Method.Equal -> equalName
    Method.NotEqual -> notEqualName

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

char =
  ( charName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index $ Type2.Char,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

typex =
  ( typeName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Type,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

constraint =
  ( constraintName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Constraint,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

small =
  ( smallName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Small,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

large =
  ( largeName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Large,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

universe =
  ( universeName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Universe,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

st =
  ( stName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index $ Type2.ST,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

runST =
  ( runSTName,
    Term.Binding
      { position = Position.internal,
        fixity = Fixity {associativity = Left, precedence = 9},
        index = Term2.RunST,
        selector = Normal
      }
  )

integer =
  ( integerName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Integer,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

int =
  ( intName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Int,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

num =
  ( numName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Num,
        constructors = Set.empty,
        fields = fields (Proxy :: Proxy Method.Num),
        methods = methods (Proxy :: Proxy Method.Num)
      }
  )

eq =
  ( eqName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Num,
        constructors = Set.empty,
        fields = fields (Proxy :: Proxy Method.Eq),
        methods = methods (Proxy :: Proxy Method.Eq)
      }
  )

plus =
  ( plusName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.plus,
        fixity = Fixity {associativity = Left, precedence = 6},
        selector = Normal
      }
  )

minus =
  ( minusName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.minus,
        fixity = Fixity {associativity = Left, precedence = 6},
        selector = Normal
      }
  )

multiply =
  ( multiplyName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.multiply,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

negate =
  ( negateName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.negate,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

abs =
  ( absName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.abs,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

signum =
  ( signumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.signum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fromInteger =
  ( fromIntegerName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fromInteger,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enum =
  ( enumName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Enum,
        constructors = Set.empty,
        fields = fields (Proxy :: Proxy Method.Enum),
        methods = methods (Proxy :: Proxy Method.Enum)
      }
  )

succ =
  ( succName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.succ,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

pred =
  ( predName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.pred,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

toEnum =
  ( toEnumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.toEnum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fromEnum =
  ( fromEnumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fromEnum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFrom =
  ( enumFromName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFrom,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromThen =
  ( enumFromThenName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromThen,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromTo =
  ( enumFromToName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromTo,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromThenTo =
  ( enumFromThenToName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromThenTo,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

equal =
  ( equalName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.equal,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
      }
  )

notEqual =
  ( notEqualName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.notEqual,
        fixity = Fixity {associativity = None, precedence = 4},
        selector = Normal
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
            equal,
            notEqual,
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
            enum,
            eq
          ],
      stability = ()
    }
