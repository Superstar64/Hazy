module Instance where

infix 4 ===

class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData

instance MyEq MyData where
  MyData === MyData = True

instance (MyEq a) => MyEq [a] where
  [] === [] = True
  (x : xs) === (x' : xs') = x === x' && xs === xs'

test = [MyData] === [MyData]
