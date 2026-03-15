module Main where

import Prelude hiding (sum)

numbers :: [Int]
numbers = [1, 3 .. 10]

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

check = case sum numbers == 25 of
  True -> "y"
  False -> "n"

main = putStrLn check
