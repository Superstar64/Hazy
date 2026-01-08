module Lambda where

id :: a -> a
id = \x -> x

match :: [Maybe ()] -> ()
match = \[Nothing, Just ()] -> ()
