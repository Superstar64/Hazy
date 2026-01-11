{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for data type brand
module Stage1.Tree.Brand where

import Stage1.Parser (Parser, token, (<|>))

data Brand
  = -- |
    --  > data T
    --  > ^^^^
    Boxed
  | -- |
    --  > newtype T
    --  > ^^^^^^^
    Newtype
  deriving (Show)

parse :: Parser Brand
parse = Boxed <$ token "data" <|> Newtype <$ token "newtype"
