module Main where

main :: IO ()
main = do
  print (abs (-3 :: Int))
  print (abs (3 :: Int))
  print (abs (0 :: Int))
  print (abs (-3 :: Integer))
  print (signum (-3 :: Int))
  print (negate (3 :: Int))
