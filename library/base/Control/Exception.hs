module Control.Exception where

-- Keep this absolutely minimal until I figure out how to deal with exceptions

class (Show e) => Exception e

throw :: (Exception e) => e -> a
throw = error "todo"

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch = error "todo"
