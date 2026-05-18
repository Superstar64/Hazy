-- |
-- Positioning data
module Stage1.Position
  ( Position,
    file,
    line,
    column,
    at,
    internal,
  )
where

import Data.Text (pack)
import Stage1.ParserCombinator (Position, at, column, file, line)

internal :: Position
internal = at (pack "<internal>:1:1")
