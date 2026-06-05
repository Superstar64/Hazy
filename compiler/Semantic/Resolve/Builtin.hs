module Semantic.Resolve.Builtin (builtin) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Binding.Term (Selector (Normal))
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Semantic.Resolve.Bindings (Bindings (..))
import Semantic.Resolve.Builtin.Applicative
  ( ap,
    applicative,
    discardLeft,
    discardRight,
    liftA2,
    pure,
  )
import Semantic.Resolve.Builtin.Bool (bool, false, true)
import Semantic.Resolve.Builtin.Enum
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
import Semantic.Resolve.Builtin.Eq (eq, equal, notEqual)
import Semantic.Resolve.Builtin.Fractional (divide, fractional, fromRational, recip)
import Semantic.Resolve.Builtin.Functor (fconst, fmap, functor)
import Semantic.Resolve.Builtin.Integral (div, divMod, integral, mod, quot, quotRem, rem, toInteger)
import Semantic.Resolve.Builtin.Monad (bind, monad, return, thenx)
import Semantic.Resolve.Builtin.MonadFail (fail, monadFail)
import Semantic.Resolve.Builtin.Num
  ( abs,
    fromInteger,
    minus,
    multiply,
    negate,
    num,
    plus,
    signum,
  )
import Semantic.Resolve.Builtin.Ord
  ( compare,
    greaterThen,
    greaterThenEqual,
    lessThen,
    lessThenEqual,
    max,
    min,
    ord,
  )
import Semantic.Resolve.Builtin.Ordering (gt, lt, ordering)
import qualified Semantic.Resolve.Builtin.Ordering as Ordering
import Semantic.Resolve.Builtin.Ratio (ratio)
import qualified Semantic.Resolve.Builtin.Ratio as Ratio
import Semantic.Resolve.Builtin.Real (real, toRational)
import Syntax.Lexer (constructorIdentifier, variableIdentifier)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Variable (..))
import Prelude hiding
  ( Applicative (..),
    Either (..),
    Enum (..),
    Fractional (..),
    Functor (..),
    Integral (..),
    Monad (..),
    MonadFail (..),
    Num (..),
    Ord (..),
    Real (..),
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

lazyName = constructorIdentifier (pack "Lazy")

strictName = constructorIdentifier (pack "Strict")

levityName = constructorIdentifier (pack "Levity")

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

lazy =
  ( lazyName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Lazy,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

strict =
  ( strictName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Strict,
        constructors = Set.empty,
        fields = Set.empty,
        methods = Map.empty
      }
  )

levity =
  ( levityName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Levity,
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
            compare,
            lessThen,
            lessThenEqual,
            greaterThen,
            greaterThenEqual,
            max,
            min,
            toRational,
            quot,
            rem,
            div,
            mod,
            quotRem,
            divMod,
            toInteger,
            divide,
            recip,
            fromRational,
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
            gt,
            Ratio.make
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
            ratio,
            num,
            enum,
            eq,
            ord,
            real,
            integral,
            fractional,
            functor,
            applicative,
            monad,
            monadFail,
            lazy,
            strict,
            levity
          ],
      stability = ()
    }
