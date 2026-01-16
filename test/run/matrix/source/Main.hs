module Main where

matrix "aa" False = "1"
matrix "aa" True = "1+"
matrix "ab" False = "2"
matrix "ab" True = "2+"
matrix "ac" False = "3"
matrix "ac" True = "3+"
matrix "ba" False = "4"
matrix "ba" True = "4+"
matrix "bb" False = "5"
matrix "bb" True = "5+"
matrix "bc" False = "6"
matrix "bc" True = "6+"
matrix "ca" False = "7"
matrix "ca" True = "7+"
matrix "cb" False = "8"
matrix "cb" True = "8+"
matrix "cc" False = "9"
matrix "cc" True = "9+"

main = putStrLn (matrix "ba" True)
