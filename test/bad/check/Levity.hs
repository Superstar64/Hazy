module Levity where

data Levity s = Levity ~!s Int

levity :: Levity s
levity = Levity 1