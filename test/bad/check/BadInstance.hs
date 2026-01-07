module BadInstance where

class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData

instance MyEq MyData where
  MyData === True = True
