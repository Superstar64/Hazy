module Main where

import Hazy.Prelude (trace)

byname :: () => Int
byname = trace "call by name" (1 + 2)

byneed :: Int
byneed = trace "call by need" (1 + 2)

main = do
  print byname
  print byname
  print byneed
  print byneed
