module Data.Heptatraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Heptafoldable (Heptafoldable)
import Data.Heptafunctor (Heptafunctor)

class (Heptafunctor t, Heptafoldable t) => Heptatraversable t where
  heptatraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    (a4 -> f b4) ->
    (a5 -> f b5) ->
    (a6 -> f b6) ->
    (a7 -> f b7) ->
    t a1 a2 a3 a4 a5 a6 a7 ->
    f (t b1 b2 b3 b4 b5 b6 b7)

heptamapDefault ::
  (Heptatraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  (a4 -> b4) ->
  (a5 -> b5) ->
  (a6 -> b6) ->
  (a7 -> b7) ->
  t a1 a2 a3 a4 a5 a6 a7 ->
  t b1 b2 b3 b4 b5 b6 b7
heptamapDefault f1 f2 f3 f4 f5 f6 f7 =
  runIdentity
    . heptatraverse
      (Identity . f1)
      (Identity . f2)
      (Identity . f3)
      (Identity . f4)
      (Identity . f5)
      (Identity . f6)
      (Identity . f7)

heptafoldMapDefault ::
  (Heptatraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  (a4 -> c) ->
  (a5 -> c) ->
  (a6 -> c) ->
  (a7 -> c) ->
  t a1 a2 a3 a4 a5 a6 a7 ->
  c
heptafoldMapDefault f1 f2 f3 f4 f5 f6 f7 =
  getConst
    . heptatraverse
      (Const . f1)
      (Const . f2)
      (Const . f3)
      (Const . f4)
      (Const . f5)
      (Const . f6)
      (Const . f7)
