module Integers where

numbers :: (Num a, Enum a) => [a]
numbers = [1, 2 ..]

integers :: [Integer]
integers = numbers

ints :: [Int]
ints = numbers
