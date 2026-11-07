module Main where

newtype Box = Box ()

ignore, repack, select :: String
ignore = case undefined of
  Box a -> "new"

newtype MyString = MyString {runMyString :: String}

repack = case MyString "ty" of
  MyString str -> str

select = runMyString (MyString "pe")

data Outer = Outer !Inner

newtype Inner = Inner Int

get :: Outer -> Int
get (Outer (Inner x)) = x

main = do
  putStrLn (ignore ++ repack ++ select)
  print (get (Outer (Inner 5)))
