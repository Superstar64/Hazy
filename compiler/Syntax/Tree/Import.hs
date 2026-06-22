{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.Import where

import Syntax.Parser (Parser, asum, betweenBuiltinPragma, position, token)
import Syntax.Position (Position)
import Syntax.Tree.Alias (Alias)
import qualified Syntax.Tree.Alias as Alias
import Syntax.Tree.ImportSymbols (Symbols)
import qualified Syntax.Tree.ImportSymbols as ImportSymbols
import Syntax.Tree.Qualification (Qualification)
import qualified Syntax.Tree.Qualification as Qualification
import Syntax.Variable (FullQualifiers)
import qualified Syntax.Variable as Variable

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
  | -- |
    -- > import {-# BUILTIN #-} M
    Builtin
      { targetPosition :: !position,
        target :: !FullQualifiers
      }

parse :: Parser (Import Position)
parse =
  token "import"
    *> asum
      [ importx
          <$> Qualification.parse
          <*> position
          <*> Variable.parse
          <*> Alias.parse
          <*> ImportSymbols.parse,
        builtin <$ betweenBuiltinPragma (pure ()) <*> position <*> Variable.parse
      ]
  where
    importx qualification targetPosition target alias symbols =
      Import
        { qualification,
          targetPosition,
          target,
          alias,
          symbols
        }
    builtin targetPosition target = Builtin {targetPosition, target}
