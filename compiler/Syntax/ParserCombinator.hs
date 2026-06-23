{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- General purpose parser combinator
module Syntax.ParserCombinator
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
import Control.Monad (ap, liftM)
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Foldable (asum)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder, fromString)
import {-# SOURCE #-} Error (expected, locate)
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
at position = Position file line column
  where
    file = Text.intercalate (pack ":") $ init $ init sections
    line = read $ Text.unpack $ last $ init sections
    column = read $ Text.unpack $ last sections
    sections = Text.splitOn (pack ":") position

step :: Position -> Char -> Position
step (Position file line _) '\n' = Position file (line + 1) 1
step (Position file line column) '\t' = Position file line (8 - (column - 1) `rem` 8 + column)
step (Position file line column) _ = Position file line (column + 1)

data Stream = Stream
  { location :: !Position,
    stream :: !Text
  }
  deriving (Show)

startStream :: Text -> Text -> Stream
startStream file stream = Stream {location, stream}
  where
    location = Position file 1 1

internal :: Text
internal = pack "<internal>"

newtype Error
  = Basic Builder
  deriving (Show)

data Lookahead stream
  = Valid stream
  | Invalid stream [Error]
  deriving (Show)

data Status stream
  = Pure
  | Error [Error]
  | Consumed (Lookahead stream)
  deriving (Show)

lookahead :: stream -> Status stream -> Lookahead stream
lookahead stream status = case status of
  Pure -> Valid stream
  Error error -> Invalid stream error
  Consumed lookahead -> lookahead

class Source stream where
  info :: stream -> Position
  found :: stream -> Builder

instance Source Stream where
  info = location
  found Stream {stream} = case Text.uncons stream of
    Just (c, _) -> fromString (show [c])
    Nothing -> Strings.endOfFile

-- If the status is `Error {}`, then value must be bottom.
data Result stream a = !(Status stream) :? a
  deriving (Show)

infix 5 :?

newtype Parser stream a = Parser (stream -> Result stream a)

abandon :: (Source stream) => stream -> [Error] -> b
abandon remaining expect = expected (info remaining) (unpack <$> expect) (found remaining)
  where
    unpack (Basic error) = error

instance (Source stream) => Functor (Parser stream) where
  fmap = liftM

instance (Source stream) => Applicative (Parser stream) where
  pure value = Parser $ \_ -> Pure :? value
  (<*>) = ap

instance (Source stream) => Monad (Parser stream) where
  Parser parser >>= after = Parser $ \stream -> case parser stream of
    status :? value
      | Parser parser <- after value -> case status of
          Pure -> parser stream
          Error error -> Error error :? abandon stream error
          Consumed look ->
            let status :? value = parser $ case look of
                  Valid stream -> stream
                  Invalid stream error -> abandon stream error
                fetch = case look of
                  Valid stream -> lookahead stream status
                  Invalid {} -> look
             in Consumed fetch :? value

instance (Source stream) => Alternative (Parser stream) where
  empty = Parser $ \stream -> Error [] :? abandon stream []
  Parser attempt <|> Parser fallback = Parser $ \stream -> case attempt stream of
    Error error1 :? _ -> case fallback stream of
      Error error2 :? _ -> Error (error1 ++ error2) :? abandon stream (error1 ++ error2)
      result -> result
    result -> result

try :: (Source stream) => Parser stream a -> Parser stream a
try (Parser parser) = Parser $ \stream -> case parser stream of
  Consumed Invalid {} :? _ -> Error [] :? abandon stream []
  result -> result

peek :: Parser stream stream
peek = Parser $ \stream -> Pure :? stream

position :: (Source stream) => Parser stream Position
position = info <$> peek

data Bind stream a
  = Parse a stream
  | Fail

satifyBind :: (Source stream) => Builder -> (stream -> Bind stream a) -> Parser stream a
satifyBind token parse = Parser $ \stream -> case parse stream of
  Parse value stream -> Consumed (Valid stream) :? value
  Fail -> Error [Basic token] :? abandon stream [Basic token]

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
stringIgnoreCase key | Text.toLower key /= key = error "bad key case"
stringIgnoreCase key = satifyBind (fromString (show key)) $ \Stream {location, stream} ->
  let size = fromIntegral $ Text.length key
   in if key == Text.toLower (Text.take size stream)
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

parse :: Parser stream a -> stream -> a
parse (Parser parser) stream = case parser stream of
  _ :? value -> value
