module Main where

value :: (Int, Char, String)
value = read "(-5, 'a', \"abc\")"

bad :: [((Int, Char), String)]
bad = readsPrec 0 "( 1 'a')"

main = do
  print value
  print bad
