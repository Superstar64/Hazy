module Main where

main = do
  print (read "123" :: Int)
  print (read "'a'" :: Char)
  print (read "'\0'" :: Char)
