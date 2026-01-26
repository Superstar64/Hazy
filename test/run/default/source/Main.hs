module Main where

class Default a where
  f, g :: a -> String
  f _ = "default"
  g = f

instance Default ()

main = putStrLn (g ())
