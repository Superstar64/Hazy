module Builtin.Real where

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
  Type2.Real
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Num a, Ord a) => Real a where
        toRational :: a -> Ratio Integer
      """
