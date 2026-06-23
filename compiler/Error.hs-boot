module Error where

import Data.Text.Lazy.Builder (Builder)
import {-# SOURCE #-} Syntax.ParserCombinator (Position)

pathCharacter, digit, octal, hexadecimal, endOfFile, character, newline, whitespace :: Builder
locate :: Position -> Builder
expected :: Position -> [Builder] -> Builder -> a
