module Main where

main :: IO ()
main = do
  print (read "(1,2)" :: (Int, Int))
  print (read "[(1,2),(3,4)]" :: [(Int, Int)])
  print (read "(1,True,'a')" :: (Int, Bool, Char))
  print (read "[]" :: [(Int, Int)])
