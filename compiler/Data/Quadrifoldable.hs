module Data.Quadrifoldable where

class Quadrifoldable f where
  quadrifoldMap ::
    (Monoid m) =>
    (a1 -> m) ->
    (a2 -> m) ->
    (a3 -> m) ->
    (a4 -> m) ->
    f a1 a2 a3 a4 ->
    m
