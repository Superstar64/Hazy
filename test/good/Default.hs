module Default where

class Default a where
  method :: a -> Int
  method _ = method ()

instance Default () where
  method () = 0
