{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for modules
module Syntax.Tree.Module (Module (..), assumeName, parse) where

import Error (mismatchedModuleNames)
import Syntax.Extensions (Extensions)
import Syntax.Parser (Parser, position, token)
import Syntax.Position (Position)
import Syntax.Tree.Declarations (Declarations)
import qualified Syntax.Tree.Declarations as Declarations
import Syntax.Tree.Exports (Exports)
import qualified Syntax.Tree.Exports as Exports
import Syntax.Variable (FullQualifiers)
import qualified Syntax.Variable as Variable
import Verbose as Null (Debug)
import qualified Verbose

data Module position = Module
  { extensions :: Extensions,
    modulePosition :: position,
    name :: !FullQualifiers,
    exports :: Exports,
    declarations :: Declarations position
  }
  deriving (Show)

assumeName :: FullQualifiers -> Module Position -> Module Position
assumeName name' ~Module {extensions, modulePosition, name, exports, declarations} =
  Module
    { name = name',
      extensions = seq valid extensions,
      modulePosition = seq valid modulePosition,
      exports = seq valid exports,
      declarations = seq valid declarations
    }
  where
    valid
      | name == name' = ()
      | otherwise = mismatchedModuleNames modulePosition

parse :: (Debug verbose) => Parser (Extensions -> verbose (Module Position))
parse = modulex <$> position <*> (token "module" *> Variable.parse) <*> Exports.parse <*> body
  where
    modulex modulePosition name exports declarations extensions =
      Verbose.parsing
        (Variable.print' name)
        Module
          { extensions,
            modulePosition,
            name,
            exports,
            declarations
          }
    body = token "where" *> Declarations.parse
