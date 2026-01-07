module WeirdClass where

class Basic a where
  basic :: a

class (Basic (f Int)) => Weird f

weird :: (Weird (g Char)) => g Char Int
weird = basic
