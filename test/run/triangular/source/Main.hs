module Main where

triangular, triangular2, triangular2' :: Integer -> Integer
triangular n
  of 
     n /= 1
     n + triangular (n - 1)
  of
    1

triangular2 n = 2 * triangular n

triangular2' n = n * (n + 1)

main :: IO ()
main = putStrLn result where
  result = case triangular2 10 == triangular2' 10 of
    True -> "y"
    False -> "n"