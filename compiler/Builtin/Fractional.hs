module Builtin.Fractional where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
(bindings, definition, extra) =
  Type2.Fractional
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Num a => Fractional a where
        (/) :: a -> a -> a
        recip :: a -> a
        fromRational :: Ratio Integer -> a

        recip x = 1 / x
        x / y = x * recip x
      infixl 7 /
      """
