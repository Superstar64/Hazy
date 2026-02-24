module Data.Quadritraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Quadrifoldable (Quadrifoldable (..))
import Data.Quadrifunctor (Quadrifunctor (..))

class (Quadrifunctor t, Quadrifoldable t) => Quadritraversable t where
  quadritraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    (a4 -> f b4) ->
    t a1 a2 a3 a4 ->
    f (t b1 b2 b3 b4)

quadrimapDefault ::
  (Quadritraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  (a4 -> b4) ->
  t a1 a2 a3 a4 ->
  t b1 b2 b3 b4
quadrimapDefault f g h i =
  runIdentity
    . quadritraverse
      (Identity . f)
      (Identity . g)
      (Identity . h)
      (Identity . i)

quadrifoldMapDefault ::
  (Quadritraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  (a4 -> c) ->
  t a1 a2 a3 a4 ->
  c
quadrifoldMapDefault f g h i =
  getConst
    . quadritraverse
      (Const . f)
      (Const . g)
      (Const . h)
      (Const . i)
