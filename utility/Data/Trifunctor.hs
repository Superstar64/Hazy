module Data.Trifunctor where

class Trifunctor f where
  trimap ::
    (a1 -> b1) ->
    (a2 -> b2) ->
    (a3 -> b3) ->
    f a1 a2 a3 ->
    f b1 b2 b3
