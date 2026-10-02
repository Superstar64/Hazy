module Hazy.PreludeText where

import Hazy.Helper
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
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "False" -> [(False, string)]
      "True" -> [(True, string)]
      _ -> []

instance (Read a) => Read (Maybe a) where
  readsPrec d string = readParen False nothing string ++ readParen (d > 10) just string
    where
      nothing string = do
        ("Nothing", string) <- lex string
        [(Nothing, string)]
      just string = do
        ("Just", string) <- lex string
        (value, string) <- readsPrec 11 string
        [(Just value, string)]

instance (Read a, Read b) => Read (Either a b) where
  readsPrec d = readParen (d > 10) $ \string -> do
    (either, string) <- lex string
    case either of
      "Left" -> do
        (value, string) <- readsPrec 11 string
        [(Left value, string)]
      "Right" -> do
        (value, string) <- readsPrec 11 string
        [(Right value, string)]
      _ -> []

instance Read Ordering where
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "LT" -> [(LT, string)]
      "EQ" -> [(EQ, string)]
      "GT" -> [(GT, string)]
      _ -> []

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

instance Read Int where
  readsPrec p r = [(fromInteger i, t) | (i, t) <- readsPrec p r]

instance Read Integer where
  readsPrec p = readSigned readDec

instance Read Float where
  readsPrec p = readSigned readFloat

instance Read Double where
  readsPrec p = readSigned readFloat

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

instance (Read a) => Read [a] where
  readsPrec p = readList

instance (Read a) => Read (NonEmpty a) where
  readsPrec = placeholder

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

instance (Read a, Integral a) => Read (Ratio a) where
  readsPrec p =
    readParen
      (p > ratPrec)
      ( \r ->
          [ (x % y, u)
          | (x, s) <- readsPrec (ratPrec + 1) r,
            ("%", t) <- lex s,
            (y, u) <- readsPrec (ratPrec + 1) t
          ]
      )
