module Stage3.Tree.MethodInfo where

newtype MethodInfo = MethodInfo
  { constraintCount :: Int
  }
  deriving (Show)
