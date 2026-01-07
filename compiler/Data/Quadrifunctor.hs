module Data.Quadrifunctor where

class Quadrifunctor f where
  quadrimap ::
    (a1 -> b1) ->
    (a2 -> b2) ->
    (a3 -> b3) ->
    (a4 -> b4) ->
    f a1 a2 a3 a4 ->
    f b1 b2 b3 b4
