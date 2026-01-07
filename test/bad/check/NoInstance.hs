module NoInstance where

class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData

instance (MyEq a) => MyEq [a]

test = [MyData] === [MyData]
