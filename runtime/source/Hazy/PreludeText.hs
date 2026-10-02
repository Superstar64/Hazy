module Hazy.PreludeText where

import Hazy.Helper
import Hazy.Prelude

type ReadS a = String -> [(a, String)]

type ShowS = String -> String

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
