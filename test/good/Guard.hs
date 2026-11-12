{-# LANGUAGE OfGuardBlocks #-}
module Guard where

multiway = f $ if
  | True -> () where f = id

corner1 = if -> ()

corner1' = ()

corner2 = if | -> ()

corner2 | = ()

guard = if | True -> ()

multiwayOf x = if
  of
    True <- x
    "valid"
  of
    otherwise
    "invalid"

functionOf x
 of
   "abc" <- x
   True
 of
   "def" <- x
   True
 | otherwise = False