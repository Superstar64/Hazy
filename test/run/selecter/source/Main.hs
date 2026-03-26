module Main where

data Uniform
  = A {a, b :: String}
  | B {a, b :: String}

data Semiuniform
  = C {c1, d :: String}
  | D {c2, d :: String}

data Disjoint
  = E {e, f, g :: String}
  | F {}

v1 = A {a = "a", b = "b"}

v2 = C {c1 = "c1", d = "d"}

v3 = E {e = "e", f = "f", g = "g"}

main = putStrLn (b v1 ++ c1 v2 ++ d v2 ++ g v3)
