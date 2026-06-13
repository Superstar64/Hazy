module Main where

import Data.Text (pack)

main = do
  print '\xd800'
  print "\xd800"
  print $ pack "\xd800"
