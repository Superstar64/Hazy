module Data.Trifoldable where

class Trifoldable f where
  trifoldMap ::
    (Monoid m) =>
    (a1 -> m) ->
    (a2 -> m) ->
    (a3 -> m) ->
    f a1 a2 a3 ->
    m
