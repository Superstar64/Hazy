module DeriveAny where

class MyClass a where
  myValue :: a

deriving instance MyClass Int
