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
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Binding.Term (Selector (Normal))
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Semantic.Resolve.Bindings (Bindings (..), BindingsF (..))
import Syntax.Lexer (constructorIdentifier, variableIdentifier)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (ConstructorIdentifier, Variable (..))
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

builtinType :: [Char] -> Type3.Index scope -> (ConstructorIdentifier, Type.Binding scope)
builtinType name index = (constructorIdentifier $ pack name, binding)
  where
    binding =
      Type.Header
        { position = Position.internal,
          constructors = Set.empty,
          fields = Set.empty
        }
        Type.:@ Identity
          Type.Binding
            { index,
              methods = Map.empty
            }

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
              [ ( VariableIdentifier $ variableIdentifier $ pack "runST",
                  Position.internal
                    Term.:@ Identity
                      Term.Binding
                        { fixity = Fixity {associativity = Left, precedence = 9},
                          index = Term2.RunST,
                          selector = Normal
                        }
                )
              ],
          constructors =
            Map.empty,
          types =
            Map.fromListWith
              undefined
              [ builtinType "Char" $ Type3.Index Type2.Char,
                builtinType "Type" Type3.Type,
                builtinType "Constraint" Type3.Constraint,
                builtinType "Small" Type3.Small,
                builtinType "Large" Type3.Large,
                builtinType "Universe" Type3.Universe,
                builtinType "ST" $ Type3.Index Type2.ST,
                builtinType "Integer" $ Type3.Index Type2.Integer,
                builtinType "Int" $ Type3.Index Type2.Int,
                builtinType "Lazy" $ Type3.Index Type2.Lazy,
                builtinType "Strict" $ Type3.Index Type2.Strict,
                builtinType "Levity" Type3.Levity
              ],
          stability = ()
        }
