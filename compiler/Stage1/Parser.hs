-- |
-- The parser applicative
module Stage1.Parser
  ( Parser,
    token,
    token',
    Integer,
    integer,
    Rational,
    decimal,
    Char,
    char,
    Text,
    text,
    peek,
    position,
    try,
    empty,
    (<|>),
    (<**>),
    between,
    betweenParens,
    betweenBrackets,
    betweenBraces,
    betweenUnorderedPragma,
    betweenFieldPragma,
    betweenBuiltinPragma,
    betweenTicks,
    optional,
    optionalParens,
    many,
    NonEmpty,
    some,
    sepBy,
    sepByComma,
    sepEndBySemicolon,
    sepBy1,
    sepBy1Comma,
    sepEndBy1Semicolon,
    sepEndBy,
    sepEndByComma,
    sepEndBy1,
    asum,
    parse,
  )
where

import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromString)
import qualified Error as Strings
import Stage1.Extensions (Extensions)
import Stage1.Lexer
  ( Lexeme,
    Lexer (..),
    formatLexeme,
    lex,
  )
import qualified Stage1.Lexer as Lexer
import Stage1.ParserCombinator
  ( Bind (..),
    between,
    empty,
    many,
    optional,
    peek,
    position,
    satifyBind,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    some,
    startStream,
    try,
    (<**>),
    (<|>),
  )
import qualified Stage1.ParserCombinator as ParserCombinator (Parser, parse)
import Prelude hiding (lex)

type Parser = ParserCombinator.Parser Lexer

token :: String -> Parser ()
token = token' . lex

token' :: Lexeme -> Parser ()
token' token = satifyBind (fromString $ show $ formatLexeme token) $ \case
  Lex _ token' lexer lexer'
    | token == token' -> Parse () lexer
    | Lexer.CloseBrace <- token -> Parse () lexer'
  _ -> Fail

integer :: Parser Integer
integer = satifyBind Strings.integer $ \case
  Lex _ (Lexer.Integer value) lexer _ -> Parse value lexer
  _ -> Fail

decimal :: Parser Rational
decimal = satifyBind Strings.decimal $ \case
  Lex _ (Lexer.Float value) lexer _ -> Parse value lexer
  _ -> Fail

char :: Parser Char
char = satifyBind Strings.character $ \case
  Lex _ (Lexer.Char value) lexer _ -> Parse value lexer
  _ -> Fail

text :: Parser Text
text = satifyBind Strings.string $ \case
  Lex _ (Lexer.String value) lexer _ -> Parse value lexer
  _ -> Fail

eof :: Parser ()
eof = satifyBind Strings.endOfFile $ \case
  end@(End _) -> Parse () end
  _ -> Fail

betweenParens,
  betweenBrackets,
  betweenBraces,
  betweenTicks ::
    Parser a -> Parser a
betweenParens = between (token "(") (token ")")
betweenBrackets = between (token "[") (token "]")
betweenBraces = between (token "{") (token "}")
betweenTicks = between (token "`") (token "`")

betweenUnorderedPragma = between (token' Lexer.Unordered) (token "#-}")

betweenBuiltinPragma = between (token' Lexer.Builtin) (token "#-}")

betweenFieldPragma = between (token' Lexer.Fields) (token "#-}")

optionalParens :: Parser a -> Parser (Maybe a)
optionalParens = optional . betweenParens

sepByComma :: Parser a -> Parser [a]
sepByComma = (`sepBy` token ",")

sepEndBySemicolon :: Parser a -> Parser [a]
sepEndBySemicolon = (`sepEndBy` token ";")

sepBy1Comma :: Parser a -> Parser (NonEmpty a)
sepBy1Comma = (`sepBy1` token ",")

sepEndBy1Semicolon :: Parser a -> Parser (NonEmpty a)
sepEndBy1Semicolon = (`sepEndBy1` token ";")

sepEndByComma :: Parser a -> Parser [a]
sepEndByComma = (`sepEndBy` token ",")

parse :: Parser (Extensions -> a) -> Text -> Text -> a
parse parser name file =
  let stream = startStream name file
      (extensions, lexer) = ParserCombinator.parse Lexer.lexer stream
   in ParserCombinator.parse (flip seq <$> parser <*> eof) lexer extensions
