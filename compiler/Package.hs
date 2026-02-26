module Package (Package (..), Module (..), load) where

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Text
import Data.Text.Lazy.Builder (fromString)
import Data.Traversable (for)
import Stage1.Lexer (FullQualifiers (..), Toggle, legalChar)
import qualified Stage1.Lexer as Lexer
import Stage1.ParserCombinator
  ( Parser,
    Stream,
    char,
    many,
    parse,
    satify,
    some,
    startStream,
    string,
    (<|>),
  )
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hSetEncoding, openFile, utf8)
import System.IO.Unsafe (unsafeInterleaveIO)

data Module = Module
  { name :: !Text,
    path :: !FullQualifiers,
    target :: FilePath,
    header :: Text,
    artifact :: Text
  }
  deriving (Show)

data Package = Package
  { extensions :: [Toggle],
    modules :: [Module]
  }
  deriving (Show)

data Path
  = Root !Text
  | !Text :</> !Path
  deriving (Show)

infixr 5 :</>

path' :: Parser Stream Path
path' = do
  head <- satify (fromString "Upper case") isUpper
  tail <- many $ satify (fromString "Legal character") legalChar
  let name = pack (head : tail)
      file = do
        string (pack ".hs")
        pure (Root name)
      folder = do
        char '/'
        subpath <- path'
        pure (name :</> subpath)
  file <|> folder

haskell :: Path -> FilePath
haskell = \case
  Root name -> unpack name ++ ".hs"
  root :</> subpath -> unpack root </> haskell subpath

javascript :: Path -> FilePath
javascript = \case
  Root name -> unpack name ++ ".mjs"
  root :</> subpath -> unpack root </> javascript subpath

qualifiers :: Path -> FullQualifiers
qualifiers = go Lexer.Local
  where
    go prefix (Root name) = prefix Lexer.:.. Lexer.constructorIdentifier name
    go prefix (root :</> subpath) = go (prefix Lexer.:. Lexer.constructorIdentifier root) subpath

data Meta = Meta
  { toggles :: [Toggle],
    paths :: [Path]
  }

white :: Parser Stream ()
white = do
  void $ some (char ' ' <|> char '\n' <|> char '\r')

meta :: Parser Stream Meta
meta = do
  toggles <- Lexer.toggles <|> pure []
  char ';'
  paths <- many (white *> path')
  pure Meta {toggles, paths}

load :: FilePath -> IO Package
load root = do
  let package = root </> "package"
  raw <- Text.readFile package
  let Meta {toggles, paths} = parse meta $ startStream (pack package) raw
  modules <- for paths $ \path -> do
    let haskellName = root </> "header" </> haskell path
        javascriptName = root </> "artifact" </> javascript path
    header <- unsafeInterleaveIO $ do
      handle <- openFile haskellName ReadMode
      hSetEncoding handle utf8
      Text.hGetContents handle
    artifact <- unsafeInterleaveIO $ do
      handle <- openFile javascriptName ReadMode
      hSetEncoding handle utf8
      Text.hGetContents handle
    pure
      Module
        { name = pack haskellName,
          path = qualifiers path,
          target = javascript path,
          header,
          artifact
        }
  pure
    Package
      { extensions = toggles,
        modules
      }
