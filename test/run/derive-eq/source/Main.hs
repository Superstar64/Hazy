module Main where

data Point = Point Int Int

deriving instance Eq Point

data Wrapper a = Wrapper a

deriving instance (Eq a) => Eq (Wrapper a)

main = do
  let point1 = Point 1 1
      point2 = Point 1 2
      wrapper1 = Wrapper "abc"
      wrapper2 = Wrapper "def"
  print $ point1 == point1
  print $ point1 == point2
  print $ wrapper1 == wrapper1
  print $ wrapper1 == wrapper2
