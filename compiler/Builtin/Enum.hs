module Builtin.Enum where

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
  Type2.Enum
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Enum a where
        succ, pred :: a -> a
        toEnum :: Int -> a
        fromEnum :: a -> Int
        enumFrom :: a -> [a]
        enumFromThen :: a -> a -> [a]
        enumFromTo :: a -> a -> [a]
        enumFromThenTo :: a -> a -> a -> [a]

        succ x = toEnum (fromEnum x + 1)
        pred x = toEnum (fromEnum x - 1)
        enumFrom x = enumFromTo x (succ x)
        enumFromTo x y = fmap toEnum [fromEnum x .. fromEnum y]
        enumFromThen x y = fmap toEnum [fromEnum x, fromEnum y ..]
        enumFromThenTo x y z = fmap toEnum [fromEnum x, fromEnum y .. fromEnum z]
      """
