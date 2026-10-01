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

deriving instance Bounded T

data Both = Both Bool Bool

instance Show Both where
  showsPrec d (Both x y) =
    showParen (d > 10) $
      showString "Both "
        . showsPrec 11 x
        . showString " "
        . showsPrec 11 y

deriving instance Bounded Both

main = do
  print $ succ A
  print $ succ B
  print $ pred G
  print [B ..]
  print [B, D .. G]
  print [D .. C]
  print [A, D ..]
  print (minBound :: T)
  print (maxBound :: T)
  print (minBound :: Both)
  print (maxBound :: Both)
