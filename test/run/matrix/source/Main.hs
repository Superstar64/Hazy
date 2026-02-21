module Main where

matrix :: String -> Integer -> Bool -> String
matrix "aa" 0 False = "1"
matrix "aa" 0 True = "2"
matrix "aa" 1 False = "3"
matrix "aa" 1 True = "4"
matrix "ab" 0 False = "5"
matrix "ab" 0 True = "6"
matrix "ab" 1 False = "7"
matrix "ab" 1 True = "8"
matrix "ba" 0 False = "9"
matrix "ba" 0 True = "10"
matrix "ba" 1 False = "11"
matrix "ba" 1 True = "12"
matrix "bb" 0 False = "13"
matrix "bb" 0 True = "14"
matrix "bb" 1 False = "15"
matrix "bb" 1 True = "16"

main = putStrLn (matrix "ba" 1 True)
