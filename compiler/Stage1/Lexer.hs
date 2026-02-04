-- |
-- The lexer
module Stage1.Lexer
  ( Lexer (..),
    lexer,
    location,
    Lexeme (..),
    formatLexeme,
    Qualifiers (..),
    FullQualifiers (..),
    toQualifiers,
    VariableIdentifier,
    variableIdentifier,
    runVariableIdentifier,
    ConstructorIdentifier,
    constructorIdentifier,
    runConstructorIdentifier,
    VariableSymbol,
    variableSymbol,
    runVariableSymbol,
    ConstructorSymbol,
    constructorSymbol,
    runConstructorSymbol,
    Category,
    lex,
  )
where

import qualified Control.Applicative as Applicative (some)
import Control.Arrow (first)
import Data.Char (chr, isDigit, isLower, isPrint, isSpace, isUpper)
import qualified Data.Char as Char (isSymbol)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Ratio ((%))
import Data.String (IsString)
import qualified Data.String as IsString (fromString)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import qualified Data.Text.Lazy.Builder as Builder
import Error (cannotTurnoffExtension)
import qualified Error as Strings
import Stage1.Extensions (Extensions (..), haskell2010, hazy)
import Stage1.ParserCombinator
  ( Bind (Fail, Parse),
    Parser,
    Position,
    Source (..),
    Stream (..),
    asum,
    char,
    column,
    digit,
    empty,
    eof,
    hexDigit,
    internal,
    line,
    many,
    octDigit,
    optional,
    parse,
    position,
    satify,
    satifyBind,
    single,
    some,
    startStream,
    step,
    string,
    stringIgnoreCase,
    try,
    (<**>),
    (<|>),
  )
import Prelude hiding (lex)

infixl 5 :., :.., :-., :=., :-:, :=:

data Lexeme
  = !Qualifiers :-. !VariableIdentifier
  | !Qualifiers :=. !ConstructorIdentifier
  | !Qualifiers :-: !VariableSymbol
  | !Qualifiers :=: !ConstructorSymbol
  | Integer !Integer
  | Float !Rational
  | Char !Char
  | String !Text
  | OpenParen
  | CloseParen
  | Comma
  | Semicolon
  | OpenBracket
  | CloseBracket
  | Backtick
  | OpenBrace
  | CloseBrace
  | Tick
  | Case
  | Class
  | Data
  | Default
  | Deriving
  | Else
  | Foreign
  | If
  | Import
  | In
  | Infix
  | Infixl
  | Infixr
  | Instance
  | Module
  | Newtype
  | Then
  | Type
  | Underscore
  | Forall
  | Let
  | Where
  | Do
  | Of
  | DotDot
  | Colon
  | ColonColon
  | Equal
  | Backslash
  | Bar
  | LeftArrow
  | RightArrow
  | At
  | Tilde
  | ThickArrow
  | Unordered
  | Fields
  | Builtin
  | ClosePragma
  deriving (Eq, Show)

data Language
  = Haskell2010
  deriving (Show, Bounded, Enum)

data Extension
  = DataKinds
  | DoAndIfThenElse
  | EmptyCase
  | EmptyDataDecls
  | FieldSelectors
  | GADTs
  | GADTSyntax
  | ExplicitForAll
  | ImplicitPrelude
  | LambdaCase
  | MonomorphismRestriction
  | MonoLocalBinds
  | MultiwayIf
  | NamedFieldPuns
  | NegativeLiterals
  | PatternGuards
  | ScopedTypeVariables
  | StandaloneKindSignatures
  | StarIsType
  | TraditionalRecordSyntax
  | TypeOperators
  | ExtendedLocalDeclarations
  | ExtendedFunctionBindings
  | ExtendedImportDeclarations
  | OfGuardBlocks
  | StableImports
  | UnorderedRecords
  | ConstructorFields
  deriving (Show, Bounded, Enum)

data Toggle
  = Language !Position !Language
  | Extension !Position !Extension
  | NoExtension !Position !Extension
  deriving (Show)

toggle :: Extensions -> Toggle -> Extensions
toggle extension = \case
  Language _ language -> case language of
    Haskell2010 -> haskell2010
  Extension _ additional -> case additional of
    ImplicitPrelude -> extension {implicitPrelude = True}
    StableImports -> extension {stableImports = True}
    UnorderedRecords -> extension {unorderedRecords = True}
    ConstructorFields -> extension {constructorFields = True}
    _ -> extension
  NoExtension position removal -> case removal of
    ImplicitPrelude -> extension {implicitPrelude = False}
    StableImports -> extension {stableImports = False}
    UnorderedRecords -> extension {unorderedRecords = False}
    ConstructorFields -> extension {constructorFields = False}
    extension -> cannotTurnoff extension
    where
      cannotTurnoff = cannotTurnoffExtension position . formatExtension

class Format e where
  format :: e -> Builder
  format = fromText . formatText
  formatText :: e -> Text
  formatText = toStrict . Builder.toLazyText . format

formatLexeme :: Lexeme -> Builder
formatLexeme = format

formatExtension :: Extension -> Builder
formatExtension = fromString . show

instance Format Lexeme where
  format = \case
    qualifiers :-. name -> format qualifiers <> format name
    qualifiers :=. name -> format qualifiers <> format name
    qualifiers :-: name -> format qualifiers <> format name
    qualifiers :=: name -> format qualifiers <> format name
    Integer value -> fromString (show value)
    Float value -> fromString (show value)
    Char value -> mconcat [Builder.singleton '\'', Builder.singleton value, Builder.singleton '\'']
    String value -> mconcat [Builder.singleton '"', fromText value, Builder.singleton '"']
    Unordered -> fromString "{-# UNORDEREDRECORDS"
    Builtin -> fromString "{-# BUILTIN"
    ClosePragma -> fromString "#-}"
    value -> fromText (formatText value)
  formatText = \case
    OpenParen -> pack "("
    CloseParen -> pack ")"
    Comma -> pack ","
    Semicolon -> pack ";"
    OpenBracket -> pack "["
    CloseBracket -> pack "]"
    Backtick -> pack "`"
    OpenBrace -> pack "{"
    CloseBrace -> pack "}"
    Tick -> pack "'"
    Case -> pack "case"
    Class -> pack "class"
    Data -> pack "data"
    Default -> pack "default"
    Deriving -> pack "deriving"
    Else -> pack "else"
    Foreign -> pack "foreign"
    If -> pack "if"
    Import -> pack "import"
    In -> pack "in"
    Infix -> pack "infix"
    Infixl -> pack "infixl"
    Infixr -> pack "infixr"
    Instance -> pack "instance"
    Module -> pack "module"
    Newtype -> pack "newtype"
    Then -> pack "then"
    Type -> pack "type"
    Underscore -> pack "_"
    Forall -> pack "forall"
    Let -> pack "let"
    Where -> pack "where"
    Do -> pack "do"
    Of -> pack "of"
    DotDot -> pack ".."
    Colon -> pack ":"
    ColonColon -> pack "::"
    Equal -> pack "="
    Backslash -> pack "\\"
    Bar -> pack "|"
    LeftArrow -> pack "<-"
    RightArrow -> pack "->"
    At -> pack "@"
    Tilde -> pack "~"
    ThickArrow -> pack "=>"
    value -> toStrict $ Builder.toLazyText $ format value

data Qualifiers
  = Local
  | !Qualifiers :. !Category
  deriving (Eq, Ord, Show)

instance Format Qualifiers where
  format = \case
    Local -> mempty
    qualifiers :. name -> mconcat [format qualifiers, format name, Builder.singleton '.']

data FullQualifiers = !Qualifiers :.. !Category
  deriving (Eq, Ord, Show)

instance Format FullQualifiers where
  format (qualifiers :.. name) = mconcat [format qualifiers, format name, Builder.singleton '.']

toQualifiers (qualifiers :.. name) = qualifiers :. name

newtype VariableIdentifier = VariableIdentifier' Text
  deriving (Eq, Ord)

runVariableIdentifier :: VariableIdentifier -> Text
runVariableIdentifier (VariableIdentifier' name) = name

variableIdentifier :: Text -> VariableIdentifier
variableIdentifier = parse (flip seq <$> varid <*> eof) . startStream internal

instance Show VariableIdentifier where
  showsPrec fixity (VariableIdentifier' text) = showsPrec fixity text

instance IsString VariableIdentifier where
  fromString = variableIdentifier . pack

instance Format VariableIdentifier where
  formatText (VariableIdentifier' name) = name

newtype ConstructorIdentifier = ConstructorIdentifier' Text
  deriving (Eq, Ord)

runConstructorIdentifier :: ConstructorIdentifier -> Text
runConstructorIdentifier (ConstructorIdentifier' name) = name

constructorIdentifier :: Text -> ConstructorIdentifier
constructorIdentifier = parse (flip seq <$> conid <*> eof) . startStream internal

instance Show ConstructorIdentifier where
  showsPrec fixity (ConstructorIdentifier' text) = showsPrec fixity text

instance IsString ConstructorIdentifier where
  fromString = constructorIdentifier . pack

instance Format ConstructorIdentifier where
  formatText (ConstructorIdentifier' name) = name

newtype VariableSymbol = VariableSymbol' Text
  deriving (Eq, Ord)

runVariableSymbol :: VariableSymbol -> Text
runVariableSymbol (VariableSymbol' name) = name

variableSymbol :: Text -> VariableSymbol
variableSymbol = parse (flip seq <$> varsym <*> eof) . startStream internal

instance Show VariableSymbol where
  showsPrec fixity (VariableSymbol' text) = showsPrec fixity text

instance IsString VariableSymbol where
  fromString = variableSymbol . pack

instance Format VariableSymbol where
  formatText (VariableSymbol' name) = name

newtype ConstructorSymbol = ConstructorSymbol' Text
  deriving (Eq, Ord)

runConstructorSymbol :: ConstructorSymbol -> Text
runConstructorSymbol (ConstructorSymbol' name) = name

constructorSymbol :: Text -> ConstructorSymbol
constructorSymbol = parse (flip seq <$> consym <*> eof) . startStream internal

instance Show ConstructorSymbol where
  showsPrec fixity (ConstructorSymbol' text) = showsPrec fixity text

instance IsString ConstructorSymbol where
  fromString = constructorSymbol . pack

instance Format ConstructorSymbol where
  formatText (ConstructorSymbol' name) = name

type Category = ConstructorIdentifier

reservedIdRelation :: [(Text, Lexeme)]
reservedIdRelation = sortOn (Down . Text.length . fst) $ [(formatText token, token) | token <- tokens]
  where
    tokens =
      [ Case,
        Class,
        Data,
        Default,
        Deriving,
        Else,
        Foreign,
        If,
        Import,
        In,
        Infix,
        Infixl,
        Infixr,
        Instance,
        Module,
        Newtype,
        Then,
        Type,
        Forall,
        Underscore,
        Let,
        Where,
        Do,
        Of
      ]

reservedSymRelation :: [(Text, Lexeme)]
reservedSymRelation = sortOn (Down . Text.length . fst) $ [(formatText token, token) | token <- tokens]
  where
    tokens =
      [ DotDot,
        Colon,
        ColonColon,
        Equal,
        Backslash,
        Bar,
        LeftArrow,
        RightArrow,
        At,
        Tilde,
        ThickArrow
      ]

specialRelation :: [(Text, Lexeme)]
specialRelation = sortOn (Down . Text.length . fst) $ [(formatText token, token) | token <- tokens]
  where
    tokens =
      [ OpenParen,
        CloseParen,
        Comma,
        Semicolon,
        OpenBracket,
        CloseBracket,
        Backtick,
        OpenBrace,
        CloseBrace,
        Tick,
        ClosePragma
      ]

showRelation :: (Show a, Bounded a, Enum a) => [(Text, a)]
showRelation = sortOn (Down . Text.length . fst) $ [(pack $ show token, token) | token <- [minBound ..]]

languageRelation :: [(Text, Language)]
languageRelation = showRelation

extensionRelation :: [(Text, Extension)]
extensionRelation = showRelation

noExtensionRelation :: [(Text, Extension)]
noExtensionRelation = [(pack "No" <> text, token) | (text, token) <- extensionRelation]

escapeRelation :: [(Text, Char)]
escapeRelation =
  sortOn
    (Down . Text.length . fst)
    $ map
      (first pack)
      [ ("a", '\a'),
        ("b", '\b'),
        ("f", '\f'),
        ("n", '\n'),
        ("r", '\r'),
        ("t", '\t'),
        ("v", '\v'),
        ("\\", '\\'),
        ("\"", '"'),
        ("'", '\''),
        ("NUL", '\NUL'),
        ("SOH", '\SOH'),
        ("STX", '\STX'),
        ("ETX", '\ETX'),
        ("EOT", '\EOT'),
        ("ENQ", '\ENQ'),
        ("ACK", '\ACK'),
        ("BEL", '\BEL'),
        ("BS", '\BS'),
        ("HT", '\HT'),
        ("LF", '\LF'),
        ("VT", '\VT'),
        ("FF", '\FF'),
        ("CR", '\CR'),
        ("SO", '\SO'),
        ("SI", '\SI'),
        ("DLE", '\DLE'),
        ("DC1", '\DC1'),
        ("DC2", '\DC2'),
        ("DC3", '\DC3'),
        ("DC4", '\DC4'),
        ("NAK", '\NAK'),
        ("SYN", '\SYN'),
        ("ETB", '\ETB'),
        ("CAN", '\CAN'),
        ("EM", '\EM'),
        ("SUB", '\SUB'),
        ("ESC", '\ESC'),
        ("FS", '\FS'),
        ("GS", '\GS'),
        ("RS", '\RS'),
        ("US", '\US'),
        ("SP", '\SP'),
        ("DEL", '\DEL'),
        ("^A", '\^A'),
        ("^B", '\^B'),
        ("^C", '\^C'),
        ("^D", '\^D'),
        ("^E", '\^E'),
        ("^F", '\^F'),
        ("^G", '\^G'),
        ("^H", '\^H'),
        ("^I", '\^I'),
        ("^J", '\^J'),
        ("^K", '\^K'),
        ("^L", '\^L'),
        ("^M", '\^M'),
        ("^N", '\^N'),
        ("^O", '\^O'),
        ("^P", '\^P'),
        ("^Q", '\^Q'),
        ("^R", '\^R'),
        ("^S", '\^S'),
        ("^T", '\^T'),
        ("^U", '\^U'),
        ("^V", '\^V'),
        ("^W", '\^W'),
        ("^X", '\^X'),
        ("^Y", '\^Y'),
        ("^Z", '\^Z'),
        ("^@", '\^@'),
        ("^[", '\^['),
        ("^\\", '\^\'),
        ("^]", '\^]'),
        ("^^", '\^^'),
        ("^_", '\^_')
      ]

reservedIdMap :: Map Text Lexeme
reservedIdMap = Map.fromList reservedIdRelation

reservedSymMap :: Map Text Lexeme
reservedSymMap = Map.fromList reservedSymRelation

isSymbol :: Char -> Bool
isSymbol letter = notSpecial && (specific || unicode)
  where
    notSpecial = all (/= Text.singleton letter) (map fst specialRelation)
    specific = elem letter "!#$%&*?./<=>?@\\^|-~:"
    unicode = Char.isSymbol letter

special :: Parser Stream Lexeme
special = asum $ map match specialRelation
  where
    match (special, token) = token <$ string special

reserved :: Parser Stream Lexeme
reserved = keys <|> syms
  where
    keys = asum $ map match reservedIdRelation
    syms = asum $ map match reservedSymRelation
    match (reserved, token) = token <$ string reserved

legalChar :: Char -> Bool
legalChar letter =
  or
    [ isLower letter,
      letter == '_',
      isUpper letter,
      isDigit letter,
      letter == '\''
    ]

varid :: Parser Stream VariableIdentifier
varid = satifyBind Strings.variableIdentifier pop
  where
    pop Stream {location, stream} = case Text.uncons stream of
      Just (letter, _)
        | isLower letter || letter == '_',
          Map.notMember name reservedIdMap ->
            Parse
              value
              Stream
                { location = Text.foldl step location name,
                  stream = Text.drop (fromIntegral $ Text.length name) stream
                }
        where
          value = VariableIdentifier' name
          name = Text.takeWhile legalChar stream
      _ -> Fail

conid :: Parser Stream ConstructorIdentifier
conid = satifyBind Strings.constructorIdentifier pop
  where
    pop Stream {location, stream} = case Text.uncons stream of
      Just (letter, _)
        | isUpper letter ->
            Parse
              value
              Stream
                { location = Text.foldl step location name,
                  stream = Text.drop (fromIntegral $ Text.length name) stream
                }
        where
          value = ConstructorIdentifier' name
          name = Text.takeWhile legalChar stream
      _ -> Fail

varsym :: Parser Stream VariableSymbol
varsym = satifyBind Strings.variableSymbol pop
  where
    pop Stream {location, stream} = case Text.uncons stream of
      Just (':', _) -> Fail
      Just (letter, _)
        | isSymbol letter,
          not $ dashes name,
          Map.notMember name reservedSymMap ->
            Parse
              value
              Stream
                { location = Text.foldl step location name,
                  stream = Text.drop (fromIntegral $ Text.length name) stream
                }
        where
          value = VariableSymbol' name
          name = Text.takeWhile isSymbol stream
          dashes string | string == pack "--" = True
          dashes string | Just ('-', cs) <- Text.uncons string = dashes cs
          dashes _ = False
      _ -> Fail

consym :: Parser Stream ConstructorSymbol
consym = satifyBind Strings.constructorSymbol pop
  where
    pop Stream {location, stream} = case Text.uncons stream of
      Just (':', _)
        | Map.notMember name reservedSymMap ->
            Parse
              value
              Stream
                { location = Text.foldl step location name,
                  stream = Text.drop (fromIntegral $ Text.length name) stream
                }
        where
          value = ConstructorSymbol' name
          name = Text.takeWhile isSymbol stream
      _ -> Fail

identifier :: Parser Stream Lexeme
identifier = ($ []) <$> identifier
  where
    identifier :: Parser Stream ([Category] -> Lexeme)
    identifier =
      asum
        [ varid' <$> varid,
          conid' <$> conid <*> property,
          varsym' <$> varsym,
          consym' <$> consym
        ]
      where
        qualify path = foldl (:.) Local (reverse path)
        varid' name path = qualify path :-. name
        conid' name Nothing path = qualify path :=. name
        conid' name (Just run) path = run (name : path)
        varsym' name path = qualify path :-: name
        consym' name path = qualify path :=: name
        property = optional (try (char '.' *> identifier))

number :: Parser Stream Lexeme
number = sign <**> (try nondecimal <|> number <**> postfix)
  where
    sign :: Parser Stream Integer
    sign = -1 <$ char '-' <|> pure 1

    number :: (Read a) => Parser Stream a
    number = read <$> Applicative.some digit
    nondecimal = parse <$> char '0' <*> asum nonbinary
      where
        nonbinary =
          [ (:) <$> asum (map char "oO") <*> Applicative.some octDigit,
            (:) <$> asum (map char "xX") <*> Applicative.some hexDigit
          ]
        parse _ digits sign = Integer $ sign * read ('0' : digits)
    exponent = try $ parse <$> char 'e' <*> sign <*> number
      where
        parse _ sign number = case sign of
          Just False -> 1 / 10 ^ number
          _ -> 10 ^ number
        sign = optional (True <$ char '+' <|> False <$ char '-')
    postfix = rational <|> raised <|> plain
      where
        rational = parse <$> char '.' <*> number <*> (exponent <|> pure 1)
          where
            parse _ decimal power base sign =
              Float $ sign % 1 * (base % 1 + read decimal % 10 ^ length decimal) * power
        raised = parse <$> exponent
          where
            parse power base sign = Float $ (sign % 1) * (base % 1) * power
        plain = pure parse
          where
            parse num sign = Integer (sign * num)

escape :: (Char -> a) -> Parser Stream a -> Parser Stream a
escape lift gap = char '\\' *> asum [gap, literals, decimal, octal, hexadecimal]
  where
    literals = asum [lift token <$ string escape | (escape, token) <- escapeRelation]
    decimal = lift . chr . read <$> Applicative.some digit
    octal = lift . chr . read . ("0o" ++) <$> Applicative.some octDigit
    hexadecimal = lift . chr . read . ("0x" ++) <$> Applicative.some hexDigit

textual :: Char -> Parser Stream Char
textual end = satify Strings.stringCharacter (\letter -> isPrint letter && letter /= end)

charText :: Parser Stream Lexeme
charText = char '\'' *> letter <* char '\''
  where
    letter = Char <$> (escape id empty <|> textual '\'')

space :: Parser Stream Char
space = satify Strings.whitespace isSpace

stringText :: Parser Stream Lexeme
stringText = char '"' *> string <* char '"'
  where
    string = String . pack . catMaybes <$> many (escape Just gap <|> Just <$> textual '"')
    gap = ampersand <|> spaces
    ampersand = Nothing <$ char '&'
    spaces = Nothing <$ some space <* char '\\'

lexeme :: Parser Stream Lexeme
-- The order here matters. There is some overlap.
lexeme = try number <|> try charText <|> special <|> identifier <|> reserved <|> stringText

lex :: String -> Lexeme
lex = parse (flip seq <$> lexeme <*> eof) . startStream internal . pack

instance IsString Lexeme where
  fromString = lex

data Premarked
  = Premark !Position !Lexeme !Position Premarked
  | Predone !Position
  deriving (Show)

newlines :: String
newlines = "\n\r\f"

comment, ncomment, ncommentBody :: Parser Stream ()
comment = try (dashes *> body)
  where
    body = newline <|> satify Strings.nonSymbol (not . isSymbol) *> line
    dashes = char '-' *> char '-' *> many (char '-')
    newline = () <$ satify Strings.newline (`elem` newlines) <|> eof
    line = newline <|> single *> line
ncomment = () <$ string (pack "{-") *> ncommentBody
ncommentBody = finish <|> nested <|> pass
  where
    nested = string (pack "{-") *> ncommentBody *> ncommentBody
    pass = single *> ncommentBody
    finish = () <$ string (pack "-}")

toggles :: Parser Stream [Toggle]
toggles =
  asum
    [ space *> toggles,
      comment *> toggles,
      pragma,
      ncomment *> toggles,
      pure []
    ]
  where
    pragma = string (pack "{-#") *> many space *> body
    body = (++) <$> known <*> toggles <|> ncommentBody *> toggles
    known = symbol *> toggle
    symbol = stringIgnoreCase (pack "language_hazy") <|> stringIgnoreCase (pack "language")
    toggle =
      asum
        [ space *> toggle,
          (:) <$> single <*> next
        ]
    next =
      asum
        [ space *> next,
          string (pack ",") *> toggle,
          done
        ]
    single =
      asum
        [ Language <$> position <*> language,
          Extension <$> position <*> extension,
          NoExtension <$> position <*> noExtension
        ]
    language = asum [token <$ string text | (text, token) <- languageRelation]
    extension = asum [token <$ string text | (text, token) <- extensionRelation]
    noExtension = asum [token <$ string text | (text, token) <- noExtensionRelation]
    done = [] <$ string (pack "#-}")

premarked :: Parser Stream Premarked
premarked =
  asum
    [ space *> premarked,
      comment *> premarked,
      pragma,
      ncomment *> premarked,
      token,
      done
    ]
  where
    pragma = position <* string (pack "{-#") <* many space <**> body
    body = known <|> const <$ ncommentBody <*> premarked
    known =
      asum
        [ record <$> (stringIgnoreCase (pack "unorderedrecords") *> position) <*> premarked,
          fields <$> (stringIgnoreCase (pack "constructorfields") *> position) <*> premarked,
          builtin <$> (stringIgnoreCase (pack "builtin") *> position) <*> premarked
        ]
    record position' premarked position = Premark position Unordered position' premarked
    builtin position' premarked position = Premark position Builtin position' premarked
    fields position' premarked position = Premark position Fields position' premarked
    token = Premark <$> position <*> lexeme <*> position <*> premarked
    done = Predone <$> position <* eof

trailingLine :: Position -> Premarked -> Bool
trailingLine position1 premark
  | Premark position2 _ _ _ <- premark = go position2
  | Predone position2 <- premark = go position2
  where
    go position2 = line position2 > line position1

data Marked
  = Token !Position !Lexeme Marked
  | Open !Position !Int Marked
  | Line !Position !Int Marked
  | Done !Position !Bool
  deriving (Show)

mark :: Premarked -> Marked
mark premark@(Premark position token _ _)
  | OpenBrace <- token = go premark
  | Module <- token = go premark
  | otherwise = Open position indent $ go premark
  where
    indent = column position
    go :: Premarked -> Marked
    go = \case
      Premark position token position' premark ->
        Token position token $
          if
            | token `elem` [Let, Where, Do, Of] -> open position' premark
            | Backslash <- token,
              Premark position Case position' premark <- premark,
              let indent = column position ->
                if trailingLine position' premark
                  then Line position' indent $ Token position Case $ open position' premark
                  else Token position Case $ open position' premark
            | If <- token, Premark _ Bar _ _ <- premark -> open position' premark
            | If <- token, Premark _ RightArrow _ _ <- premark -> open position' premark
            | If <- token, Premark _ Of _ _ <- premark -> open position' premark
            | trailingLine position' premark,
              Premark position _ _ _ <- premark,
              let indent = column position ->
                Line position' indent (go premark)
            | otherwise -> go premark
        where
          open position' premark = case premark of
            Premark position token' _ _
              | token' /= OpenBrace ->
                  let indent = column position
                   in Open position' indent (go premark)
              | otherwise -> go premark
            Predone position -> Done position True
      Predone position -> Done position False
mark (Predone position) = Done position False

data Lexer
  = -- lexing haskell is nondeterministic due to `}` insertion
    -- second lexer should be looked at when a second `}` is expected
    -- but not found
    Lex !Position !Lexeme Lexer Lexer
  | End !Position
  | ErrorBrace !Position
  | ErrorParen !Position
  | ErrorBracket !Position
  | ErrorPragma !Position

instance Source Lexer where
  info = \case
    Lex position _ _ _ -> position
    End position -> position
    ErrorBrace position -> position
    ErrorParen position -> position
    ErrorBracket position -> position
    ErrorPragma position -> position
  found = \case
    Lex _ token _ _ -> fromString $ show (format token)
    End _ -> Strings.endOfFile
    ErrorBrace _ -> Strings.braceIdentionError
    ErrorParen _ -> Strings.parenIdentionError
    ErrorBracket _ -> Strings.bracketIdentionError
    ErrorPragma _ -> Strings.pragmaIdentionError

-- extend layout rules to support not insert braces inside of parens and brackets
-- this allows code like this
{-
f x = [
  1
]
-}
data Brace
  = Indent !Int
  | Brace
  | Paren
  | Bracket
  | Pragma
  deriving (Eq)

layout :: [Brace] -> Marked -> Lexer
layout (Indent m : ms) (Line position n ts)
  | m == n = Lex position Semicolon (layout (Indent m : ms) ts) (ErrorBrace position)
  | n < m = Lex position CloseBrace (layout ms (Line position n ts)) (ErrorBrace position)
layout ms (Line _ _ ts) = layout ms ts
layout ms (Open position n ts)
  | legal ms = Lex position OpenBrace (layout (Indent n : ms) ts) (ErrorBrace position)
  where
    -- indention can decrease when surrounded by explicit braces
    -- seems odd, but Haskell report seems to imply this is correct
    legal (Indent m : _) = n > m
    legal [] = True
    legal (Brace : _) = True
    legal (_ : ms) = legal ms
layout ms (Open position n ts) =
  Lex
    position
    OpenBrace
    (Lex position CloseBrace (layout ms (Line position n ts)) (ErrorBrace position))
    (ErrorBrace position)
layout ms ts@(Token position CloseBrace ts') = case ms of
  (Brace : ms) -> Lex position CloseBrace (layout ms ts') (ErrorBrace position)
  (Indent _ : ms) -> Lex position CloseBrace (layout ms ts) (ErrorBrace position)
  _ -> ErrorBrace position
layout ms ts@(Token position CloseParen ts') = case ms of
  (Paren : ms) -> Lex position CloseParen (layout ms ts') (ErrorBrace position)
  (Indent _ : ms) -> Lex position CloseBrace (layout ms ts) (ErrorBrace position)
  _ -> ErrorParen position
layout ms ts@(Token position CloseBracket ts') = case ms of
  (Bracket : ms) -> Lex position CloseBracket (layout ms ts') (ErrorBrace position)
  (Indent _ : ms) -> Lex position CloseBrace (layout ms ts) (ErrorBrace position)
  _ -> ErrorBracket position
layout ms ts@(Token position ClosePragma ts') = case ms of
  (Pragma : ms) -> Lex position ClosePragma (layout ms ts') (ErrorBrace position)
  (Indent _ : ms) -> Lex position CloseBrace (layout ms ts) (ErrorBrace position)
  _ -> ErrorPragma position
layout ms (Token position OpenBrace ts) = Lex position OpenBrace (layout (Brace : ms) ts) (ErrorBrace position)
layout ms (Token position OpenParen ts) = Lex position OpenParen (layout (Paren : ms) ts) $ case ms of
  (Indent _ : ms') -> layout ms' (Token position OpenParen ts)
  _ -> ErrorBrace position
layout ms (Token position OpenBracket ts) = Lex position OpenBracket (layout (Bracket : ms) ts) $ case ms of
  (Indent _ : ms') -> layout ms' (Token position OpenBracket ts)
  _ -> ErrorBrace position
layout ms (Token position token ts) | pragma = Lex position token (layout (Pragma : ms) ts) $ case ms of
  (Indent _ : ms') -> layout ms' (Token position token ts)
  _ -> ErrorBrace position
  where
    pragma = case token of
      Unordered -> True
      Builtin -> True
      Fields -> True
      _ -> False
layout ms ts@(Token position t ts') = Lex position t (layout ms ts') $ case ms of
  (Indent _ : ms') -> layout ms' ts
  _ -> ErrorBrace position
layout (Indent _ : ms) ts@(Done position False) = Lex position CloseBrace (layout ms ts) (ErrorBrace position)
layout (Brace : _) (Done position _) = ErrorBrace position
layout (Paren : _) (Done position _) = ErrorParen position
layout (Bracket : _) (Done position _) = ErrorBracket position
layout (Pragma : _) (Done position _) = ErrorPragma position
layout [] (Done position False) = End position
layout ms (Done position True) =
  Lex
    position
    OpenBrace
    (Lex position CloseBrace (layout ms (Done position False)) (ErrorBrace position))
    (ErrorBrace position)

lexer :: Parser Stream (Extensions, Lexer)
lexer = do
  extensions <- toggles
  premark <- premarked
  let marked = mark premark
      rendered = layout [] marked
  pure (foldl toggle hazy extensions, rendered)
