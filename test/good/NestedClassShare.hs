module NestedClassShare where

f x y = (x, y) where
  class A a where
    g :: a
    g = seq (x == x) undefined

  class B a where
    h :: a
    h = seq (x :: [Int]) undefined
  
  instance A () where
    g = seq (y == y) undefined
  
  instance B () where
    h = seq (y :: [Int]) undefined