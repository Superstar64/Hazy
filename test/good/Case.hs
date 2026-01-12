module Case where

data Color = Red | Blue | Green | Custom Int

code :: Color -> Int
code color = case color of
  Red -> 0xff
  Green -> 0xff00
  Blue -> 0xff0000
  Custom color -> color
