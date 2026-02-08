module Generalization where

poly x y = y x

specific1 :: Int -> (Int -> a) -> a
specific1 = poly

specific2 :: Char -> (Char -> a) -> a
specific2 = poly
