module Data.Octafoldable where

class Octafoldable f where
  octafoldMap ::
    (Monoid m) =>
    (a1 -> m) ->
    (a2 -> m) ->
    (a3 -> m) ->
    (a4 -> m) ->
    (a5 -> m) ->
    (a6 -> m) ->
    (a7 -> m) ->
    (a8 -> m) ->
    f a1 a2 a3 a4 a5 a6 a7 a8 ->
    m
