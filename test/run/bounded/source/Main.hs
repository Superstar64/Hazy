module Main where

main = do
  print (minBound :: (Bool, (Char, Int), Ordering))
  print (maxBound :: (Bool, (Char, Int), Ordering))
