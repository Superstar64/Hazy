module Main where

import Data.Semigroup (stimes)

example, example', example2 :: (Ordering, Int -> String, IO String)
example = (GT, \x -> show x, pure "b")
example' = mempty <> example <> mempty
example2 = stimes (2 :: Int) example

check1
  | (GT, _, _) <- example',
    (GT, _, _) <- example2 =
      putStrLn "True"
  | otherwise = putStrLn "False"

check2
  | (_, lam1, _) <- example',
    (_, lam2, _) <- example2 = do
      putStrLn $ lam1 5
      putStrLn $ lam2 5

check3
  | (_, _, io1) <- example',
    (_, _, io2) <- example2 = do
      io1 >>= print
      io2 >>= print

main = do
  check1
  check2
  check3
