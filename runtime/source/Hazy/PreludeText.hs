module Hazy.PreludeText where

import Hazy.Prelude

type ReadS a = String -> [(a, String)]

type ShowS = String -> String

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readList =
    readParen
      False
      ( \r ->
          [ pr
          | ("[", s) <- lex r,
            pr <- readl s
          ]
      )
    where
      readl s =
        [([], t) | ("]", t) <- lex s]
          ++ [ (x : xs, u)
             | (x, t) <- reads s,
               (xs, u) <- readl' t
             ]
      readl' s =
        [([], t) | ("]", t) <- lex s]
          ++ [ (x : xs, v)
             | (",", t) <- lex s,
               (x, u) <- reads t,
               (xs, v) <- readl' u
             ]

instance Read Bool where
  readsPrec = placeholder

instance (Read a) => Read (Maybe a) where
  readsPrec = placeholder

instance (Read a, Read b) => Read (Either a b) where
  readsPrec = placeholder

instance Read Ordering where
  readsPrec = placeholder

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

  showsPrec _ x s = show x ++ s

  show x = showsPrec 0 x ""

  showList [] = showString "[]"
  showList (x : xs) = showChar '[' . shows x . showl xs
    where
      showl [] = showChar ']'
      showl (x : xs) =
        showChar ','
          . shows x
          . showl xs

instance Show Bool where
  showsPrec = placeholder

instance (Show a) => Show (Maybe a) where
  showsPrec = placeholder

instance (Show a, Show b) => Show (Either a b) where
  showsPrec = placeholder

instance Show Ordering where
  showsPrec = placeholder

reads :: (Read a) => ReadS a
reads = readsPrec 0

shows :: (Show a) => a -> ShowS
shows = showsPrec 0

read :: (Read a) => String -> a
read s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> x
  [] -> error "Prelude.read: no parse"
  _ -> error "Prelude.read: ambiguous parse"

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

readParen :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
  where
    optional r = g r ++ mandatory r
    mandatory r =
      [ (x, u)
      | ("(", s) <- lex r,
        (x, t) <- optional s,
        (")", u) <- lex t
      ]

lex :: ReadS String
lex "" = [("", "")]
lex (c : s)
  | isSpace c = lex (dropWhile isSpace s)
lex ('\'' : s) =
  [ ('\'' : ch ++ "'", t)
  | (ch, '\'' : t) <- lexLitChar s,
    ch /= "'"
  ]
lex ('"' : s) = [('"' : str, t) | (str, t) <- lexString s]
  where
    lexString ('"' : s) = [("\"", s)]
    lexString s =
      [ (ch ++ str, u)
      | (ch, t) <- lexStrItem s,
        (str, u) <- lexString t
      ]

    lexStrItem ('\\' : '&' : s) = [("\\&", s)]
    lexStrItem ('\\' : c : s)
      | isSpace c =
          [ ("\\&", t)
          | '\\' : t <-
              [dropWhile isSpace s]
          ]
    lexStrItem s = lexLitChar s
lex (c : s)
  | isSingle c = [([c], s)]
  | isSym c = [(c : sym, t) | (sym, t) <- [span isSym s]]
  | isAlpha c = [(c : nam, t) | (nam, t) <- [span isIdChar s]]
  | isDigit c =
      [ (c : ds ++ fe, t)
      | (ds, s) <- [span isDigit s],
        (fe, t) <- lexFracExp s
      ]
  | otherwise = []
  where
    isSingle c = c `elem` ",;()[]{}_`"
    isSym c = c `elem` "!@#$%&*+./<=>?\\^|:-~"
    isIdChar c = isAlphaNum c || c `elem` "_'"

    lexFracExp ('.' : c : cs)
      | isDigit c =
          [ ('.' : ds ++ e, u)
          | (ds, t) <- lexDigits (c : cs),
            (e, u) <- lexExp t
          ]
    lexFracExp s = lexExp s

    lexExp (e : s)
      | e `elem` "eE" =
          [ (e : c : ds, u)
          | (c : t) <- [s],
            c `elem` "+-",
            (ds, u) <- lexDigits t
          ]
            ++ [(e : ds, t) | (ds, t) <- lexDigits s]
    lexExp s = [("", s)]

instance Show Int where
  showsPrec n = showsPrec n . toInteger

instance Read Int where
  readsPrec p r = [(fromInteger i, t) | (i, t) <- readsPrec p r]

instance Show Integer where
  showsPrec = showSigned showInt

instance Read Integer where
  readsPrec p = readSigned readDec

instance Show Float where
  showsPrec p = showFloat

instance Read Float where
  readsPrec p = readSigned readFloat

instance Show Double where
  showsPrec p = showFloat

instance Read Double where
  readsPrec p = readSigned readFloat

instance Show () where
  showsPrec p () = showString "()"

instance Read () where
  readsPrec p =
    readParen
      False
      ( \r ->
          [ ((), t)
          | ("(", s) <- lex r,
            (")", t) <- lex s
          ]
      )

instance Show Char where
  showsPrec p '\'' = showString "'\\''"
  showsPrec p c = showChar '\'' . showLitChar c . showChar '\''

  showList cs = showChar '"' . showl cs
    where
      showl "" = showChar '"'
      showl ('"' : cs) = showString "\\\"" . showl cs
      showl (c : cs) = showLitChar c . showl cs

instance Read Char where
  readsPrec p =
    readParen
      False
      ( \r ->
          [ (c, t)
          | ('\'' : s, t) <- lex r,
            (c, "\'") <- readLitChar s
          ]
      )

  readList =
    readParen
      False
      ( \r ->
          [ (l, t)
          | ('"' : s, t) <- lex r,
            (l, _) <- readl s
          ]
      )
    where
      readl ('"' : s) = [("", s)]
      readl ('\\' : '&' : s) = readl s
      readl s =
        [ (c : cs, u)
        | (c, t) <- readLitChar s,
          (cs, u) <- readl t
        ]

instance (Show a) => Show [a] where
  showsPrec p = showList

instance (Read a) => Read [a] where
  readsPrec p = readList

instance (Show a, Show b) => Show (a, b) where
  showsPrec p (x, y) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ')'

instance (Read a, Read b) => Read (a, b) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b), s6)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (")", s6) <- lex s5
          ]
      )

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (x, y, z) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ','
      . shows z
      . showChar ')'

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b, c), s8)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (",", s6) <- lex s5,
            (c, s7) <- reads s6,
            (")", s8) <- lex s7
          ]
      )

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (x, y, z, k) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ','
      . shows z
      . showChar ','
      . shows k
      . showChar ')'

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b, c, d), s10)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (",", s6) <- lex s5,
            (c, s7) <- reads s6,
            (",", s8) <- lex s7,
            (d, s9) <- reads s8,
            (")", s10) <- lex s9
          ]
      )

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (x, y, z, k, m) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ','
      . shows z
      . showChar ','
      . shows k
      . showChar ','
      . shows m
      . showChar ')'

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b, c, d, e), s12)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (",", s6) <- lex s5,
            (c, s7) <- reads s6,
            (",", s8) <- lex s7,
            (d, s9) <- reads s8,
            (",", s10) <- lex s9,
            (e, s11) <- reads s10,
            (")", s12) <- lex s11
          ]
      )

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
  showsPrec _ (x, y, z, k, m, n) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ','
      . shows z
      . showChar ','
      . shows k
      . showChar ','
      . shows m
      . showChar ','
      . shows n
      . showChar ')'

instance (Read a, Read b, Read c, Read d, Read e, Read f) => Read (a, b, c, d, e, f) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b, c, d, e, f), s14)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (",", s6) <- lex s5,
            (c, s7) <- reads s6,
            (",", s8) <- lex s7,
            (d, s9) <- reads s8,
            (",", s10) <- lex s9,
            (e, s11) <- reads s10,
            (",", s12) <- lex s11,
            (f, s13) <- reads s12,
            (")", s14) <- lex s13
          ]
      )

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
  showsPrec _ (x, y, z, k, m, n, p) =
    showChar '('
      . shows x
      . showChar ','
      . shows y
      . showChar ','
      . shows z
      . showChar ','
      . shows k
      . showChar ','
      . shows m
      . showChar ','
      . shows n
      . showChar ','
      . shows p
      . showChar ')'

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g) => Read (a, b, c, d, e, f, g) where
  readsPrec _ =
    readParen
      False
      ( \s1 ->
          [ ((a, b, c, d, e, f, g), s16)
          | ("(", s2) <- lex s1,
            (a, s3) <- reads s2,
            (",", s4) <- lex s3,
            (b, s5) <- reads s4,
            (",", s6) <- lex s5,
            (c, s7) <- reads s6,
            (",", s8) <- lex s7,
            (d, s9) <- reads s8,
            (",", s10) <- lex s9,
            (e, s11) <- reads s10,
            (",", s12) <- lex s11,
            (f, s13) <- reads s12,
            (",", s14) <- lex s13,
            (g, s15) <- reads s14,
            (")", s16) <- lex s15
          ]
      )
