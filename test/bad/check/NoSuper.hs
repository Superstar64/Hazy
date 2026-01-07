module NoSuper where

data MyType = MyType

instance Ord MyType where
  MyType `compare` MyType = EQ
