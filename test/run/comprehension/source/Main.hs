module Main where

items :: [Int]
items = [x + y | x <- [0 .. 10], y <- [0 .. 10], x == y]

total :: [Int] -> Int
total [] = 0
total (x : xs) = x + total xs

main = putStrLn $ case total items of
  110 -> "y"
  _ -> "n"
