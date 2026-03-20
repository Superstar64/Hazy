module Main where

number :: Integer
number = 1

data Custom = Custom

instance Eq Custom where
  Custom == Custom = True

same = case (number, 'a', Custom) == (1, 'a', Custom) of
  True -> "y"
  False -> "n"

main = putStrLn same
