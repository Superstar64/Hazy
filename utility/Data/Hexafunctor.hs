module Data.Hexafunctor where

class Hexafunctor f where
  hexamap ::
    (a1 -> b1) ->
    (a2 -> b2) ->
    (a3 -> b3) ->
    (a4 -> b4) ->
    (a5 -> b5) ->
    (a6 -> b6) ->
    f a1 a2 a3 a4 a5 a6 ->
    f b1 b2 b3 b4 b5 b6
