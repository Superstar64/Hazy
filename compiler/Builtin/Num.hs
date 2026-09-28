module Builtin.Num where

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
  Type2.Num
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Num a where
        (+), (-), (*) :: a -> a -> a
        negate, abs, signum :: a -> a
        fromInteger :: Integer -> a

        x - y = x + negate y
        negate x = fromInteger 0 - x
      infixl 6 +, -
      infixl 7 *
      """
