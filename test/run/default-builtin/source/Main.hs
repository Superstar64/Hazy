module Main where

data MyBool = MyFalse | MyTrue

instance Eq MyBool where
  MyFalse == MyFalse = True
  MyTrue == MyTrue = True
  _ == _ = False

main =
  putStrLn
    ( case MyFalse /= MyTrue of
        True -> "y"
        False -> "n"
    )
