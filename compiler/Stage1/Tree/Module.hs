{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for modules
module Stage1.Tree.Module (Module (..), assumeName, parse) where

import Error (mismatchedModuleNames)
import Stage1.Extensions (Extensions)
import Stage1.Parser (Parser, position, token)
import Stage1.Position (Position)
import Stage1.Tree.Declarations (Declarations)
import qualified Stage1.Tree.Declarations as Declarations
import Stage1.Tree.Exports (Exports)
import qualified Stage1.Tree.Exports as Exports
import Stage1.Variable (FullQualifiers)
import qualified Stage1.Variable as Variable
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
