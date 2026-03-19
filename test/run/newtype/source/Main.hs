module Main where

newtype Box = Box ()

ignore, repack, select :: String
ignore = case undefined of
  Box a -> "new"

newtype MyString = MyString {runMyString :: String}

repack = case MyString "ty" of
  MyString str -> str

select = runMyString (MyString "pe")

combine :: String -> String -> String
combine [] ys = ys
combine (x : xs) ys = x : combine xs ys

main = putStrLn (ignore `combine` repack `combine` select)
