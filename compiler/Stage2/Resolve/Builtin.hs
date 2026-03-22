module Stage2.Resolve.Builtin (builtin) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (..))
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Stage2.Resolve.Bindings (Bindings (..))
import Stage2.Resolve.Builtin.Applicative
  ( ap,
    applicative,
    discardLeft,
    discardRight,
    liftA2,
    pure,
  )
import Stage2.Resolve.Builtin.Bool (bool, false, true)
import Stage2.Resolve.Builtin.Enum
  ( enum,
    enumFrom,
    enumFromThen,
    enumFromThenTo,
    enumFromTo,
    fromEnum,
    pred,
    succ,
    toEnum,
  )
import Stage2.Resolve.Builtin.Eq (eq, equal, notEqual)
import Stage2.Resolve.Builtin.Functor (fconst, fmap, functor)
import Stage2.Resolve.Builtin.Monad (bind, monad, return, thenx)
import Stage2.Resolve.Builtin.MonadFail (fail, monadFail)
import Stage2.Resolve.Builtin.Num
  ( abs,
    fromInteger,
    minus,
    multiply,
    negate,
    num,
    plus,
    signum,
  )
import Stage2.Resolve.Builtin.Ordering (gt, lt, ordering)
import qualified Stage2.Resolve.Builtin.Ordering as Ordering
import Prelude hiding
  ( Either (Left, Right),
    abs,
    enumFrom,
    enumFromThen,
    enumFromThenTo,
    enumFromTo,
    fail,
    fmap,
    fromEnum,
    fromInteger,
    liftA2,
    negate,
    pred,
    pure,
    return,
    signum,
    succ,
    toEnum,
  )

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
            runST,
            fmap,
            fconst,
            pure,
            ap,
            liftA2,
            discardLeft,
            discardRight,
            bind,
            thenx,
            return,
            fail
          ],
      constructors =
        Map.fromListWith
          undefined
          [ false,
            true,
            lt,
            Ordering.eq,
            gt
          ],
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
            ordering,
            num,
            enum,
            eq,
            functor,
            applicative,
            monad,
            monadFail
          ],
      stability = ()
    }
