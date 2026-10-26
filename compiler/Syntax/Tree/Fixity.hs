{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for operator fixity
module Syntax.Tree.Fixity where

import Data.Maybe (fromMaybe)
import Syntax.Parser (Parser, asum, integer, optional, token)
import Prelude hiding (Either (Left, Right))

data Fixity
  = -- |
    -- > infix 9 `x`
    -- > ^^^^^^^
    Fixity
    { associativity :: !Associativity,
      precedence :: !Int
    }
  deriving (Show)

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
  deriving (Show, Eq)

parse :: Parser Fixity
parse =
  fixity
    <$> asum
      [ Left <$ token "infixl",
        Right <$ token "infixr",
        None <$ token "infix"
      ]
    <*> (fromMaybe 9 <$> optional (fromInteger <$> integer))
  where
    fixity associativity precedence = Fixity {associativity, precedence}
