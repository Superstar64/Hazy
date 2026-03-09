module Stage3.Tree.ConstructorInfo where

newtype ConstructorInfo = ConstructorInfo
  { parameterCount :: Int
  }
  deriving (Show)
