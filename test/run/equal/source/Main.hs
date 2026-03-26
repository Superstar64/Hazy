module Main where

number :: Integer
number = 1

data Custom = Custom

instance Eq Custom where
  Custom == Custom = True

same = case (number, True, Custom, "abc", EQ) == (1, True, Custom, "abc", EQ) of
  True -> "y"
  False -> "n"

main = putStrLn same
