module Main where

newtype Box = Box ()

ignore, repack, select :: String
ignore = case undefined of
  Box a -> "new"

newtype MyString = MyString {runMyString :: String}

repack = case MyString "ty" of
  MyString str -> str

select = runMyString (MyString "pe")

main = putStrLn (ignore ++ repack ++ select)
