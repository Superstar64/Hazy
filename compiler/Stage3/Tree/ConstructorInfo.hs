module Stage3.Tree.ConstructorInfo where

data ConstructorInfo
  = ConstructorInfo
      { parameterCount_ :: !Int
      }
  | Newtype
  deriving (Show)

parameterCount :: ConstructorInfo -> Int
parameterCount ConstructorInfo {parameterCount_} = parameterCount_
parameterCount Newtype = 1
