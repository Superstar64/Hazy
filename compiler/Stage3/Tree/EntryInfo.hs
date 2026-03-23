module Stage3.Tree.EntryInfo where

newtype EntryInfo = EntryInfo
  { strict :: Bool
  }
  deriving (Show)
