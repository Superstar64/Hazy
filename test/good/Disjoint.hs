module Disjoint where

data Disjoint
  = One {x :: Int}
  | Two {y :: String}

selector :: Disjoint -> Int
selector dis = x dis
