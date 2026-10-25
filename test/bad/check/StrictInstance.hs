module StrictInstance where

data MyData = MyData

instance !(Eq MyData) where
  MyData == MyData = True
