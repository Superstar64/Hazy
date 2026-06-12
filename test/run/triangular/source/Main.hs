module Main where

triangular, triangular' :: Integer -> Integer
triangular n
  of 
     n /= 1
     n + triangular (n - 1)
  of
    1

triangular' n = n * (n + 1) `quot` 2

main :: IO ()
main =
  print $ triangular 10 == triangular' 10
