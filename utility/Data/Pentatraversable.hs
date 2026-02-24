module Data.Pentatraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Pentafoldable (Pentafoldable)
import Data.Pentafunctor (Pentafunctor)

class (Pentafunctor t, Pentafoldable t) => Pentatraversable t where
  pentatraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    (a4 -> f b4) ->
    (a5 -> f b5) ->
    t a1 a2 a3 a4 a5 ->
    f (t b1 b2 b3 b4 b5)

pentamapDefault ::
  (Pentatraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  (a4 -> b4) ->
  (a5 -> b5) ->
  t a1 a2 a3 a4 a5 ->
  t b1 b2 b3 b4 b5
pentamapDefault f g h i j =
  runIdentity
    . pentatraverse
      (Identity . f)
      (Identity . g)
      (Identity . h)
      (Identity . i)
      (Identity . j)

pentafoldMapDefault ::
  (Pentatraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  (a4 -> c) ->
  (a5 -> c) ->
  t a1 a2 a3 a4 a5 ->
  c
pentafoldMapDefault f g h i j =
  getConst
    . pentatraverse
      (Const . f)
      (Const . g)
      (Const . h)
      (Const . i)
      (Const . j)
