module Main where

import Data.Ratio (denominator, numerator)

literal = case 0.5 :: Rational of
  number
    | numerator number == 1 && denominator number == 2 -> "y"
    | otherwise -> "n"

add = case 0.1 + 0.5 :: Rational of
  number
    | numerator number == 3 && denominator number == 5 -> "y"
    | otherwise -> "n"

main = putStrLn (literal ++ add)
