module Main where

number :: Integer
number = 1

data Custom = Custom

instance Eq Custom where
  Custom == Custom = True

instance Ord Custom where
  Custom `compare` Custom = EQ

same = (number, True, Custom, "abc", EQ) == (1, True, Custom, "abc", EQ)

less = (number, True, Custom, "abc", EQ) < (1, True, Custom, "abc", GT)

greater = (number, True, Custom, "abd", EQ) > (1, True, Custom, "abc", EQ)

main = do
  print same
  print less
  print greater
