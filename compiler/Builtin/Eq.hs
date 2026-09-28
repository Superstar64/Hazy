module Builtin.Eq where

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
  Type2.Eq
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Eq a where
        (==), (/=) :: a -> a -> Bool
        x == y = if x /= y
          then False
          else True
        x /= y = if x == y
          then False
          else True
      infix 4 ==, /=
      """
