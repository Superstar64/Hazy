module Promotion where

type Promoted :: Bool -> *
data Promoted phantom = Promoted

sample :: Promoted 'True
sample = Promoted

type Promoted2 :: [Bool] -> *
data Promoted2 phantom = Promoted2

sample2 :: Promoted2 '[ 'True, 'False]
sample2 = Promoted2
