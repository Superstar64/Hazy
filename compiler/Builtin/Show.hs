module Builtin.Show where

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
  Type2.Show
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Show a where
        showsPrec :: Int -> a -> [Char] -> [Char]
        show :: a -> [Char]
        showList :: [a] -> [Char] -> [Char]

        showsPrec _ x s = show x <> s

        show x = showsPrec 0 x ""

        showList [] = (<>) "[]"
        showList (x : xs) s = (:) '[' (showsPrec 0 x (showl xs s))
          where
            showl [] = (:) ']'
            showl (x : xs) s = (:) ',' (showsPrec 0 x (showl xs s))
      """
