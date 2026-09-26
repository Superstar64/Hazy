module Promotion where

import Data.Kind (Constraint)

type Promoted :: Bool -> *
data Promoted phantom = Promoted

sample :: Promoted 'True
sample = Promoted

type Promoted2 :: [Bool] -> *
data Promoted2 phantom = Promoted2

sample2 :: Promoted2 '[ 'True, 'False]
sample2 = Promoted2

type Generate :: Bool -> Constraint
class Generate a where
  generate :: Promoted a

instance Generate 'True where
  generate = sample

usage1 :: (Generate a) => Promoted a
usage1 = generate

usage2 :: Promoted 'True
usage2 = generate
