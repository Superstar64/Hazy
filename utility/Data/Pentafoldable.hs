module Data.Pentafoldable where

class Pentafoldable f where
  pentafoldMap ::
    (Monoid m) =>
    (a1 -> m) ->
    (a2 -> m) ->
    (a3 -> m) ->
    (a4 -> m) ->
    (a5 -> m) ->
    f a1 a2 a3 a4 a5 ->
    m
