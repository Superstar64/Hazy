-- |
-- Positioning data
module Syntax.Position
  ( Position,
    file,
    line,
    column,
    at,
    internal,
  )
where

import Data.Text (pack)
import Syntax.ParserCombinator (Position, at, column, file, line)

internal :: Position
internal = at (pack "<internal>:1:1")
