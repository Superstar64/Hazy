-- |
-- Positioning data
module Stage1.Position
  ( Position,
    file,
    line,
    column,
    at,
    internal,
    Is (..),
    Equal (..),
  )
where

import Data.Text (pack)
import Stage1.ParserCombinator (Position, at, column, file, line)

internal :: Position
internal = at (pack "<internal>:1:1")

data Equal position where
  Equal :: Equal Position

instance Is Position where
  refl = Equal

class Is position where
  refl :: Equal position
