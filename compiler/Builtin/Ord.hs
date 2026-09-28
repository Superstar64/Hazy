module Builtin.Ord where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
(bindings, definition, extra) =
  Type2.Ord
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<), (<=), (>), (>=) :: a -> a -> Bool
        max, min :: a -> a -> a

        compare x y
          | x == y = EQ
          | x <= y = LT
          | True = GT

        x <= y = compare x y /= GT
        x < y = compare x y == LT
        x >= y = compare x y /= LT
        x > y = compare x y == GT

        max x y
          | x <= y = y
          | True = x
        min x y
          | x <= y = x
          | True = y
      infix 4 <, <=, >, >=
      """
