{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for import data fields
module Syntax.Tree.ImportFields where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser
  ( Parser,
    asum,
    betweenParens,
    sepByComma,
    token,
    try,
  )
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked)
import qualified Syntax.Tree.Marked as Marked
import Syntax.Variable (Name)

data Fields
  = -- |
    -- > import M ( C ( a ) )
    -- >              ^^^^^
    Fields {picks :: !(Strict.Vector (Marked Name Position))}
  | AllFields
  deriving (Show)

parse :: Parser Fields
parse =
  asum
    [ AllFields <$ try (betweenParens (token "..")),
      fields <$> betweenParens (Strict.Vector.fromList <$> sepByComma Marked.parseLiteral),
      pure (fields Strict.Vector.empty)
    ]
  where
    fields picks = Fields {picks}
