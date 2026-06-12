module Main where

data MyBool = MyFalse | MyTrue

instance Eq MyBool where
  MyFalse == MyFalse = True
  MyTrue == MyTrue = True
  _ == _ = False

main = print (MyFalse /= MyTrue)
