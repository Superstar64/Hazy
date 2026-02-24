module Data.Hexafoldable where

class Hexafoldable f where
  hexafoldMap ::
    (Monoid m) =>
    (a1 -> m) ->
    (a2 -> m) ->
    (a3 -> m) ->
    (a4 -> m) ->
    (a5 -> m) ->
    (a6 -> m) ->
    f a1 a2 a3 a4 a5 a6 ->
    m
