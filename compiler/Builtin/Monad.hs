module Builtin.Monad where

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
  Type2.Monad
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Applicative m => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
        return :: a -> m a

        x >> y = x >>= \\_ -> y
      infixl 1 >>=, >>
      """
