module Builtin.Integral where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (..))
import qualified Core.Tree.Instanciation as Instanciation
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
(bindings, definition) =
  Type2.Integral
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Real a, Enum a) => Integral a where
        quot :: a -> a -> a
        rem :: a -> a -> a
        div :: a -> a -> a
        mod :: a -> a -> a
        quotRem :: a -> a -> (a, a)
        divMod :: a -> a -> (a, a)
        toInteger :: a -> Integer
      infixl 7 `quot`, `rem`, `div`, `mod`
      """

extra :: ClassExtra scope
extra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go integral =
          Hook {hook = DefaultIntegral {integral, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
