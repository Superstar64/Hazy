module Main where

main = case undefined of
  ~(_, _) -> case "ir" of
    ~(i : r : []) ->
      putStrLn (i : r : "refutable")
