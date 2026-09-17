module Semantic.Resolve.Functor2 where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor2 as Proper (Functor2 (..))
import Data.Kind (Constraint, Type)
import Data.NaturalTransformation (NaturalTransformation (..))
import Data.Traversable2 (fmap2Default)
import qualified Data.Traversable2 as Proper (Traversable2 (..))
import Semantic.Scope (Environment)

fmap2 :: (Traversable2 binding) => NaturalTransformation f b -> binding f scope -> binding b scope
fmap2 (Morph f) x = runIdentity $ traverse2 (Morph $ Compose . Identity . f) x

type Traversable2 :: ((Type -> Type) -> Environment -> Type) -> Constraint
class Traversable2 binding where
  traverse2 :: (Applicative f) => NaturalTransformation a (Compose f b) -> binding a scope -> f (binding b scope)

type Proper :: ((Type -> Type) -> Environment -> Type) -> Environment -> (Type -> Type) -> Type
newtype Proper binding scope loeb = Proper {runProper :: binding loeb scope}

instance (Traversable2 binding) => Proper.Functor2 (Proper binding scope) where
  fmap2 = fmap2Default

instance (Traversable2 binding) => Proper.Traversable2 (Proper binding scope) where
  traverse2 f (Proper bindings) = fmap Proper $ traverse2 f bindings
