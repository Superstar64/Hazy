module Main where

type Peano :: *
data Peano = Zero | Succ Peano

length :: Peano -> String
length = \case
  Succ peano -> '+' : length peano
  Zero -> ""

class Double a where
  double :: a -> a

instance Double Peano where
  double (Succ peano) = Succ (Succ (double peano))
  double Zero = Zero

quadruple :: (Double a) => a -> a
quadruple peano = double (double peano)

three, twelve :: Peano
three = Succ (Succ (Succ Zero))
twelve = quadruple three

main :: IO ()
main = putStrLn (length twelve)
