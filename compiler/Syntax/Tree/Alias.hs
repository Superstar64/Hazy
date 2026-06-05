{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for import renames
module Syntax.Tree.Alias (Alias (..), parse) where

import Syntax.Parser (Parser, optional, token)
import Syntax.Variable (FullQualifiers)
import qualified Syntax.Variable as Variable

data Alias
  = -- |
    -- Import rename
    --
    -- > import A as B
    -- >        ^^^^^^
    Alias {name :: !FullQualifiers}
  | NoAlias
  deriving (Show)

parse :: Parser Alias
parse = scope <$> optional (token "as" *> Variable.parse)
  where
    scope = \case
      Nothing -> NoAlias
      Just name -> Alias {name}
