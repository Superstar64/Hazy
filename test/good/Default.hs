module Default where

class Default a where
  method, more :: a -> Int
  method _ = method ()
  more = method

instance Default () where
  method () = 0
