{-# LANGUAGE_HAZY UnorderedRecords #-}
module Stage1.Tree.Import where

import Stage1.Parser (Parser, position, token)
import Stage1.Position (Position)
import Stage1.Tree.Alias (Alias)
import qualified Stage1.Tree.Alias as Alias
import Stage1.Tree.ImportSymbols (Symbols)
import qualified Stage1.Tree.ImportSymbols as ImportSymbols
import Stage1.Tree.Qualification (Qualification)
import qualified Stage1.Tree.Qualification as Qualification
import Stage1.Variable (FullQualifiers)
import qualified Stage1.Variable as Variable

data Import position
  = -- |
    --  > import M
    Import
    { qualification :: !Qualification,
      targetPosition :: !position,
      target :: !FullQualifiers,
      alias :: !Alias,
      symbols :: !Symbols
    }

parse :: Parser (Import Position)
parse =
  importx
    <$ token "import"
    <*> Qualification.parse
    <*> position
    <*> Variable.parse
    <*> Alias.parse
    <*> ImportSymbols.parse
  where
    importx qualification targetPosition target alias symbols =
      Import
        { qualification,
          targetPosition,
          target,
          alias,
          symbols
        }
