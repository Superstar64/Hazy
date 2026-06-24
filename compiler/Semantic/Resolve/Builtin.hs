module Semantic.Resolve.Builtin (builtin) where

import {-# SOURCE #-} qualified Builtin.Applicative as Applicative
import {-# SOURCE #-} qualified Builtin.Bool as Bool
import {-# SOURCE #-} qualified Builtin.Enum as Enum
import {-# SOURCE #-} qualified Builtin.Eq as Eq
import {-# SOURCE #-} qualified Builtin.Fractional as Fractional
import {-# SOURCE #-} qualified Builtin.Functor as Functor
import {-# SOURCE #-} qualified Builtin.Integral as Integral
import {-# SOURCE #-} qualified Builtin.List as List
import {-# SOURCE #-} qualified Builtin.Monad as Monad
import {-# SOURCE #-} qualified Builtin.MonadFail as MonadFail
import {-# SOURCE #-} qualified Builtin.Num as Num
import {-# SOURCE #-} qualified Builtin.Ord as Ord
import {-# SOURCE #-} qualified Builtin.Ordering as Ordering
import {-# SOURCE #-} qualified Builtin.Ratio as Ratio
import {-# SOURCE #-} qualified Builtin.Real as Real
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
  mconcat
    [ baseline,
      Applicative.bindings,
      Bool.bindings,
      Enum.bindings,
      Eq.bindings,
      Fractional.bindings,
      Functor.bindings,
      Integral.bindings,
      List.bindings,
      Monad.bindings,
      MonadFail.bindings,
      Num.bindings,
      Ord.bindings,
      Ordering.bindings,
      Ratio.bindings,
      Real.bindings
    ]
  where
    baseline =
      Bindings
        { terms =
            Map.fromListWith
              undefined
              [ runST
              ],
          constructors =
            Map.empty,
          types =
            Map.fromListWith
              undefined
              [ char,
                typex,
                constraint,
                small,
                large,
                universe,
                st,
                integer,
                int,
                lazy,
                strict,
                levity
              ],
          stability = ()
        }
