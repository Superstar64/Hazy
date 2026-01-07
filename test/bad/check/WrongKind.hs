module WrongWind where

type Wrong :: * -> *
class Wrong a where
  wrong :: a -> a
