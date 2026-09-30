module Main where

data T = A | B | C | D | E | F | G | H

instance Show T where
  show = \case
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    G -> "G"
    H -> "H"

deriving instance Enum T

main = do
  print $ succ A
  print $ succ B
  print $ pred G
  -- print [ B ..]
  print [B, D .. G]
  print [D .. C]

-- print [ A, D ..]
