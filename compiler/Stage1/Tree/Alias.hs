-- |
-- Parser syntax tree for import renames
module Stage1.Tree.Alias (Alias (..), parse) where

import Stage1.Parser (Parser, optional, token)
import Stage1.Variable (FullQualifiers)
import qualified Stage1.Variable as Variable

data Alias
  = -- |
    -- Import rename
    --
    -- > import A as B
    -- >        ^^^^^^
    Alias !FullQualifiers
  | NoAlias
  deriving (Show)

parse :: Parser Alias
parse = scope <$> optional (token "as" *> Variable.parse)
  where
    scope = \case
      Nothing -> NoAlias
      Just name -> Alias name
