{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Syntax tree for operator associativity
module Stage1.Tree.Associativity where

import Stage1.Parser (Parser, asum, token)
import Prelude hiding (Either (Left, Right))

data Associativity
  = -- | Left associativity
    --
    -- > infixl
    Left
  | -- | Right associativity
    --
    -- > infixl
    Right
  | -- | No associativity
    --
    -- > infix
    None
  deriving (Show, Read, Eq)

parse :: Parser Associativity
parse =
  asum
    [ Left <$ token "infixl",
      Right <$ token "infixr",
      None <$ token "infix"
    ]
