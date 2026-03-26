module Main where

number :: Integer
number = 1

data Custom = Custom

instance Eq Custom where
  Custom == Custom = True

instance Ord Custom where
  Custom `compare` Custom = EQ

same = case (number, True, Custom, "abc", EQ) == (1, True, Custom, "abc", EQ) of
  True -> "y"
  False -> "n"

less = case (number, True, Custom, "abc", EQ) < (1, True, Custom, "abc", GT) of
  True -> "y"
  False -> "n"

greater = case (number, True, Custom, "abd", EQ) > (1, True, Custom, "abc", EQ) of
  True -> "y"
  False -> "n"

main = putStrLn (same ++ less ++ greater)
