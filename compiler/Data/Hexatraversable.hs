module Data.Hexatraversable where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Hexafoldable (Hexafoldable)
import Data.Hexafunctor (Hexafunctor)

class (Hexafunctor t, Hexafoldable t) => Hexatraversable t where
  hexatraverse ::
    (Applicative f) =>
    (a1 -> f b1) ->
    (a2 -> f b2) ->
    (a3 -> f b3) ->
    (a4 -> f b4) ->
    (a5 -> f b5) ->
    (a6 -> f b6) ->
    t a1 a2 a3 a4 a5 a6 ->
    f (t b1 b2 b3 b4 b5 b6)

hexamapDefault ::
  (Hexatraversable t) =>
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  (a4 -> b4) ->
  (a5 -> b5) ->
  (a6 -> b6) ->
  t a1 a2 a3 a4 a5 a6 ->
  t b1 b2 b3 b4 b5 b6
hexamapDefault f g h i j k =
  runIdentity
    . hexatraverse
      (Identity . f)
      (Identity . g)
      (Identity . h)
      (Identity . i)
      (Identity . j)
      (Identity . k)

hexafoldMapDefault ::
  (Hexatraversable t, Monoid c) =>
  (a1 -> c) ->
  (a2 -> c) ->
  (a3 -> c) ->
  (a4 -> c) ->
  (a5 -> c) ->
  (a6 -> c) ->
  t a1 a2 a3 a4 a5 a6 ->
  c
hexafoldMapDefault f g h i j k =
  getConst
    . hexatraverse
      (Const . f)
      (Const . g)
      (Const . h)
      (Const . i)
      (Const . j)
      (Const . k)
