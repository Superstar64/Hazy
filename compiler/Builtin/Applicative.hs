module Builtin.Applicative where

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
  Type2.Applicative
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        liftA2 :: (a -> b -> c) -> f a -> f b -> f c
        (*>) :: f a -> f b -> f b
        (<*) :: f a -> f b -> f a

        (<*>) = liftA2 (\\x -> x)
        liftA2 f a b = fmap f a <*> b
        (*>) = liftA2 (\\_ y -> y)
        (<*) = liftA2 (\\x _ -> x)
      infixl 4 <*>, *>, <*
      """
