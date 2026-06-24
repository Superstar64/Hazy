module Builtin.Num where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (DefaultNum))
import qualified Core.Tree.Hook
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
  Type2.Num
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Num a where
        (+), (-), (*) :: a -> a -> a
        negate, abs, signum :: a -> a
        fromInteger :: Integer -> a
      infixl 6 +, -
      infixl 7 *
      """

extra :: ClassExtra scope
extra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go num =
          Hook {hook = DefaultNum {num, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
