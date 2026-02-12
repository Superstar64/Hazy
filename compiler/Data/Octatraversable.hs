module Data.Octatraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Octafoldable (Octafoldable)
import Data.Octafunctor (Octafunctor)

class (Octafunctor t, Octafoldable t) => Octatraversable t where
  octatraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    (a4 -> f b4) ->
    (a5 -> f b5) ->
    (a6 -> f b6) ->
    (a7 -> f b7) ->
    (a8 -> f b8) ->
    t a1 a2 a3 a4 a5 a6 a7 a8 ->
    f (t b1 b2 b3 b4 b5 b6 b7 b8)

octamapDefault ::
  (Octatraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  (a4 -> b4) ->
  (a5 -> b5) ->
  (a6 -> b6) ->
  (a7 -> b7) ->
  (a8 -> b8) ->
  t a1 a2 a3 a4 a5 a6 a7 a8 ->
  t b1 b2 b3 b4 b5 b6 b7 b8
octamapDefault f1 f2 f3 f4 f5 f6 f7 f8 =
  runIdentity
    . octatraverse
      (Identity . f1)
      (Identity . f2)
      (Identity . f3)
      (Identity . f4)
      (Identity . f5)
      (Identity . f6)
      (Identity . f7)
      (Identity . f8)

octafoldMapDefault ::
  (Octatraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  (a4 -> c) ->
  (a5 -> c) ->
  (a6 -> c) ->
  (a7 -> c) ->
  (a8 -> c) ->
  t a1 a2 a3 a4 a5 a6 a7 a8 ->
  c
octafoldMapDefault f1 f2 f3 f4 f5 f6 f7 f8 =
  getConst
    . octatraverse
      (Const . f1)
      (Const . f2)
      (Const . f3)
      (Const . f4)
      (Const . f5)
      (Const . f6)
      (Const . f7)
      (Const . f8)
