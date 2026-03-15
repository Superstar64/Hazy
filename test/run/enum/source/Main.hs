module Main where

import Prelude hiding (sum)

numbers :: (Enum a, Num a) => [a]
numbers = [1, 3 .. 10]

sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

numbersInt :: [Int]
numbersInt = numbers

numbersInteger :: [Integer]
numbersInteger = numbers

checkInt = case sum numbersInt == 25 of
  True -> 'y'
  False -> 'n'

checkInteger = case sum numbersInteger == 25 of
  True -> 'y'
  False -> 'n'

main = putStrLn (checkInt : checkInteger : [])
