module Builtin.Semigroup where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
-- todo reintroduce error message
-- error "Prelude.stimes: negative number"

(bindings, definition, extra) =
  Type2.Semigroup
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Semigroup a where
        (<>) :: a -> a -> a
        a <> b = sconcat (a :| [b])

        sconcat :: NonEmpty a -> a
        sconcat = \\case
          x :| [] -> x
          x :| x' : xs -> x <> sconcat (x' :| xs)

        stimes :: (Integral b) => b -> a -> a
        stimes n a | n >= 1 = go n a
          where
            go 1 a = a
            go n a = a <> go (n - 1) a
      infixr 6 <>
      """
