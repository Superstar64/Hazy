-- |
-- Parser syntax tree for import data fields
module Stage1.Tree.ImportFields where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    sepByComma,
    token,
    try,
  )
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked)
import qualified Stage1.Tree.Marked as Marked
import Stage1.Variable (Name)

data Fields
  = -- |
    -- > import M ( C ( a ) )
    -- >              ^^^^^
    Fields !(Strict.Vector (Marked Name Position))
  | AllFields
  deriving (Show)

parse :: Parser Fields
parse =
  asum
    [ AllFields <$ try (betweenParens (token "..")),
      Fields <$> betweenParens (Strict.Vector.fromList <$> sepByComma Marked.parseLiteral),
      pure (Fields Strict.Vector.empty)
    ]
