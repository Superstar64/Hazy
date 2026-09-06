module Semantic.Functor2 where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor2 as Proper (Functor2 (..))
import Data.Kind (Constraint, Type)
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Traversable2 as Proper (Traversable2 (..))
import Semantic.Layout (Layout)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

fmap2 ::
  (Traversable2 ast) =>
  NaturalTransformation a b ->
  ast a locality layout stage scope ->
  ast b locality layout stage scope
fmap2 (Morph f) x = runIdentity $ traverse2 (Morph $ Compose . Identity . f) x

type Traversable2 :: ((Type -> Type) -> Locality -> Layout -> Stage -> Environment -> Type) -> Constraint
class Traversable2 ast where
  traverse2 ::
    (Applicative f) =>
    NaturalTransformation a (Compose f b) ->
    ast a locality layout stage scope ->
    f (ast b locality layout stage scope)

type Proper ::
  ((Type -> Type) -> Locality -> Layout -> Stage -> Environment -> Type) ->
  Locality ->
  Layout ->
  Stage ->
  Environment ->
  (Type -> Type) ->
  Type
newtype Proper ast locality layout stage scope loeb = Proper
  { runProper :: ast loeb locality layout stage scope
  }

instance (Traversable2 ast) => Proper.Functor2 (Proper ast locality layout stage scope) where
  fmap2 f (Proper ast) = Proper $ fmap2 f ast

instance (Traversable2 ast) => Proper.Traversable2 (Proper ast locality layout stage scope) where
  traverse2 f (Proper ast) = Proper <$> traverse2 f ast
