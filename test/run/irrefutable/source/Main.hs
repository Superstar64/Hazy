module Main where

data StrictBox = StrictBox !Int

force1 :: StrictBox -> Int
force1 ~(StrictBox x) = x

data Outer = Outer Inner

data Inner = Inner !Int !Char

force2 :: Outer -> Int
force2 ~(Outer ~(Inner x _)) = x

main = case undefined of
  ~(_, _) -> case "ir" of
    ~(i : r : []) -> do
      putStrLn (i : r : 'r' : 'e' : 'f' : 'u' : t : a : b : l : e : [])
      print (force1 (StrictBox 10))
      print (force2 (Outer (Inner 11 'a')))

t : a : b : l : e : [] = "table"
