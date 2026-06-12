module Main where

import Data.Ratio (denominator, numerator)

literal =
  let number :: Rational
      number = 0.5
   in numerator number == 1 && denominator number == 2

add =
  let number :: Rational
      number = 0.1 + 0.5
   in numerator number == 3 && denominator number == 5

main = do
  print [literal, add]
  print (0.5 :: Rational)
