module Main where

import Prelude hiding (sum)

numbers :: (Enum a, Num a) => [a]
numbers = [1, 3 .. 10]

main = do
  print (numbers :: [Int])
  print (numbers :: [Integer])
