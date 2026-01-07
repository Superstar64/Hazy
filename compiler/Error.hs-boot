module Error where

import Data.Text.Lazy.Builder (Builder)
import {-# SOURCE #-} Stage1.ParserCombinator (Position)

pathCharacter, digit, octal, hexadecimal, endOfFile, character :: Builder
locate :: Position -> Builder
locateHint :: Position -> Builder -> Builder
expected :: Position -> [Builder] -> Builder -> a
