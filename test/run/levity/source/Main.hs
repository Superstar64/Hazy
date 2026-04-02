{-# LANGUAGE_HAZY NoStableImports #-}
module Main where

import Hazy.Prelude
import Prelude ()

type List :: Levity -> Type -> Type
data List s a
  = Nil
  | Cons
      { head :: ~!s a,
        tail :: ~!s (List s a)
      }
strictSingleton :: List Strict Char
strictSingleton = Cons 'a' Nil

lazySingleton :: List Lazy Char
lazySingleton = Cons 'a' Nil

inferredSingleton = Cons 'a' Nil

class Stringify levity where
  stringify :: List levity Char -> String
  stringify Nil = []
  stringify Cons { head, tail } = head : stringify tail

instance Stringify Strict
instance Stringify Lazy

main = do
  putStrLn (stringify strictSingleton)
  putStrLn (stringify lazySingleton)
  putStrLn (stringify inferredSingleton)