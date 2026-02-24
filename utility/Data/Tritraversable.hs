module Data.Tritraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))

class (Trifunctor t, Trifoldable t) => Tritraversable t where
  tritraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    t a1 a2 a3 ->
    f (t b1 b2 b3)

trimapDefault ::
  (Tritraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  t a1 a2 a3 ->
  t b1 b2 b3
trimapDefault f g h =
  runIdentity
    . tritraverse
      (Identity . f)
      (Identity . g)
      (Identity . h)

trifoldMapDefault ::
  (Tritraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  t a1 a2 a3 ->
  c
trifoldMapDefault f g h =
  getConst
    . tritraverse
      (Const . f)
      (Const . g)
      (Const . h)
