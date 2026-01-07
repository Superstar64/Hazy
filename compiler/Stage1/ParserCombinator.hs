-- |
-- General purpose parser combinator
module Stage1.ParserCombinator
  ( Position,
    file,
    line,
    column,
    at,
    step,
    Stream (..),
    startStream,
    internal,
    Source (..),
    Parser,
    peek,
    position,
    empty,
    (<|>),
    (<**>),
    many,
    some,
    optional,
    between,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    asum,
    try,
    Bind (..),
    satifyBind,
    satify,
    char,
    string,
    stringIgnoreCase,
    single,
    eof,
    digit,
    octDigit,
    hexDigit,
    parse,
  )
where

import Control.Applicative (Alternative, (<**>))
import qualified Control.Applicative ((<|>))
import Control.Applicative.Combinators (between, empty, many, optional, sepBy, sepEndBy)
import Control.Applicative.Combinators.NonEmpty (sepBy1, sepEndBy1, some)
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Foldable (asum)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder, fromString)
import {-# SOURCE #-} Error (expected, locate, locateHint)
import {-# SOURCE #-} qualified Error as Strings

-- right associativity is better for lazyness
infixr 3 <|>

(<|>) :: (Alternative f) => f a -> f a -> f a
(<|>) = (Control.Applicative.<|>)

data Position = Position !Text !Int !Int

file :: Position -> Text
file (Position file _ _) = file

line :: Position -> Int
line (Position _ line _) = line

column :: Position -> Int
column (Position _ _ column) = column

instance Show Position where
  showsPrec fixity position = showParen (fixity > 10) $ showString "at " . showsPrec 11 (locate position)

at :: Text -> Position
at = parse at . startStream internal
  where
    at :: Parser Stream Position
    at = parse <$> section <*> char ':' <*> number <*> char ':' <*> number <*> eof
      where
        parse file _ line _ column ()
          | file <- pack file,
            line <- read line,
            column <- read column =
              Position file line column
        section = many (satify Strings.pathCharacter (/= ':'))
        number = many digit

step :: Position -> Char -> Position
step (Position file line _) '\n' = Position file (line + 1) 1
step (Position file line column) '\t' = Position file line (column + 8)
step (Position file line column) _ = Position file line (column + 1)

data Stream = Stream
  { location :: !Position,
    stream :: !Text
  }
  deriving (Show)

startStream :: Text -> Text -> Stream
startStream file stream = Stream {location, stream}
  where
    location
      | not $ ':' `Text.elem` file = Position file 1 1
      | otherwise = error "unexpected colon in position"

internal :: Text
internal = pack "<internal>"

data Error
  = Basic !Builder
  | Try !Int !Int !Builder
  deriving (Eq, Ord, Show)

data Lookahead
  = Valid
  | Invalid [Error]
  deriving (Show)

data Status
  = Pure [Error]
  | Error [Error]
  | Consumed Lookahead
  deriving (Show)

lookahead :: Status -> Lookahead
lookahead status = case status of
  Pure {} -> Valid
  Error error -> Invalid error
  Consumed lookahead -> lookahead

class Source s where
  info :: s -> Position
  found :: s -> Builder

instance Source Stream where
  info = location
  found Stream {stream} = case Text.uncons stream of
    Just (c, _) -> fromString (show [c])
    Nothing -> Strings.endOfFile

{-
If `status` is `Error _`, then `value` must be bottom.
This is needed to ensure `p <|> empty = p`, as `<|>` only evaluates it's second argument when `status` is `Error`.
-}
data Result s a = Result
  { status :: !Status,
    value :: a,
    remaining :: s
  }
  deriving (Show)

newtype Parser s a = Parser (s -> Result s a)

errorResult :: (Source s) => s -> [Error] -> Result s a
errorResult stream expect = Result (Error expect) bottom stream
  where
    bottom :: a
    bottom = expected (info stream) (format <$> expect) (found stream)
      where
        format = \case
          Basic error -> error
          Try line column error ->
            let Position file _ _ = info stream
             in locateHint (Position file line column) error

instance (Source s) => Functor (Parser s) where
  fmap f (Parser parser) = Parser $ \stream -> case parser stream of
    Result (Error error) _ stream -> errorResult stream error
    Result status x stream -> Result status (f x) stream

instance (Source s) => Applicative (Parser s) where
  pure a = Parser $ \stream -> Result (Pure []) a stream
  Parser parser <*> Parser parser' = Parser $ \stream -> case parser stream of
    Result (Error error) _ stream -> errorResult stream error
    Result (Pure error1) f stream -> case parser' stream of
      Result (Pure error2) a stream -> Result (Pure (error1 ++ error2)) (f a) stream
      Result (Error error2) _ stream -> errorResult stream (error1 ++ error2)
      Result status x stream -> Result status (f x) stream
    Result (Consumed a) f stream -> Result (Consumed (lookahead status)) (f x) stream'
      where
        Result status x stream' = case a of
          Valid -> parser' stream
          Invalid error -> errorResult stream error

instance (Source s) => Monad (Parser s) where
  return = pure
  Parser parser >>= after = Parser $ \stream -> case parser stream of
    Result (Error error) _ stream -> errorResult stream error
    Result (Pure error1) value stream
      | Parser parser <- after value -> case parser stream of
          Result (Pure error2) value stream -> Result (Pure (error1 ++ error2)) value stream
          Result (Error error2) _ stream -> errorResult stream (error1 ++ error2)
          Result status value stream -> Result status value stream
    Result (Consumed look) value stream -> Result (Consumed (lookahead status)) value' stream'
      where
        Result status value' stream' = case look of
          Valid
            | Parser parser <- after value -> parser stream
          Invalid error -> errorResult stream error

instance (Source s) => Alternative (Parser s) where
  empty = Parser $ \stream -> errorResult stream mempty
  Parser parser <|> Parser parser' = Parser $ \stream -> case parser stream of
    result@Result {status = Consumed _} -> result
    result@Result {status = Pure {}} -> result
    Result {status = Error error1} -> case parser' stream of
      Result {status = Error error2} -> errorResult stream (error1 ++ error2)
      Result (Pure error2) a stream -> Result (Pure $ error1 ++ error2) a stream
      result -> result

try :: (Source s) => Parser s a -> Parser s a
try (Parser parser) = Parser $ \stream -> case parser stream of
  Result {status = Consumed (Invalid error), remaining} -> errorResult stream (map raise error)
    where
      Position _ line column = info remaining
      raise = \case
        Basic error -> Try line column error
        error@Try {} -> error
  result -> result

peek :: Parser s s
peek = Parser $ \stream -> Result (Pure []) stream stream

position :: (Source s) => Parser s Position
position = info <$> peek

data Bind s a
  = Parse a s
  | Fail

satifyBind :: (Source s) => Builder -> (s -> Bind s a) -> Parser s a
satifyBind token parse = Parser $ \stream -> case parse stream of
  Parse a stream -> Result (Consumed Valid) a stream
  Fail -> errorResult stream [Basic token]

satify :: Builder -> (Char -> Bool) -> Parser Stream Char
satify token legal = satifyBind token $ \Stream {location, stream} -> case Text.uncons stream of
  Just (letter, stream) | legal letter -> Parse letter Stream {location = step location letter, stream}
  _ -> Fail

char :: Char -> Parser Stream Char
char letter = satify (fromString (show [letter])) (== letter)

string :: Text -> Parser Stream Text
string key = satifyBind (fromString (show key)) $ \Stream {location, stream} ->
  let size = fromIntegral $ Text.length key
   in if key == Text.take size stream
        then Parse key Stream {location = Text.foldl step location key, stream = Text.drop size stream}
        else Fail

stringIgnoreCase :: Text -> Parser Stream Text
stringIgnoreCase key = satifyBind (fromString (show key)) $ \Stream {location, stream} ->
  let size = fromIntegral $ Text.length key
   in if Text.toLower key == Text.toLower (Text.take size stream)
        then Parse key Stream {location = Text.foldl step location key, stream = Text.drop size stream}
        else Fail

single :: Parser Stream Char
single = satify Strings.character (const True)

eof :: Parser Stream ()
eof = satifyBind Strings.endOfFile $ \Stream {location, stream} ->
  if Text.null stream
    then Parse () Stream {location, stream}
    else Fail

digit, octDigit, hexDigit :: Parser Stream Char
digit = satify Strings.digit isDigit
octDigit = satify Strings.octal isOctDigit
hexDigit = satify Strings.hexadecimal isHexDigit

parse :: Parser s a -> s -> a
parse (Parser parser) stream = value (parser stream)
