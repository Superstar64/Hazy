module Main where

data T
  = A
  | B Int Char
  | Int `C` Char
  | D {x :: Int, (%^) :: Char}

deriving instance Show T

main = do
  print A
  print (B 1 'a')
  print (C 2 'b')
  print (D 3 'c')
  putStrLn $ showsPrec 11 A ""
  putStrLn $ showsPrec 11 (B 1 'a') ""
  putStrLn $ showsPrec 11 (C 2 'b') ""
  putStrLn $ showsPrec 11 (D 3 'c') ""
