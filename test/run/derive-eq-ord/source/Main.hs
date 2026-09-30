module Main where

data Point
  = Point Int Int
  | Special

deriving instance Eq Point

deriving instance Ord Point

data Wrapper a = Wrapper a

deriving instance (Eq a) => Eq (Wrapper a)

deriving instance (Ord a) => Ord (Wrapper a)

newtype Wrapper2 a = Wrapper2 a

deriving instance (Eq a) => Eq (Wrapper2 a)

deriving instance (Ord a) => Ord (Wrapper2 a)

main = do
  let point1 = Point 1 1
      point2 = Point 1 2
      point3 = Special
      wrapper1 = Wrapper "abc"
      wrapper2 = Wrapper "def"
      wrapper3 = Wrapper2 "abc"
      wrapper4 = Wrapper2 "def"
  print $ point1 == point1
  print $ point1 `compare` point1
  print $ point1 == point2
  print $ point1 `compare` point2
  print $ point1 == point3
  print $ point1 `compare` point3
  print $ wrapper1 == wrapper1
  print $ wrapper1 `compare` wrapper1
  print $ wrapper1 == wrapper2
  print $ wrapper1 `compare` wrapper2
  print $ wrapper3 == wrapper3
  print $ wrapper3 `compare` wrapper3
  print $ wrapper3 == wrapper4
  print $ wrapper3 `compare` wrapper4
