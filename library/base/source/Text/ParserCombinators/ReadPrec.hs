module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    lift,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_P,
    readP_to_Prec,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Data.Char (Char)
import Data.Int (Int)
import Data.String (String)
import Hazy.Prelude (placeholder)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read (ReadS)
import Prelude (error)

data ReadPrec a

type Prec = Int

minPrec :: Prec
minPrec = placeholder

lift :: ReadP a -> ReadPrec a
lift = placeholder

prec :: Prec -> ReadPrec a -> ReadPrec a
prec = placeholder

step :: ReadPrec a -> ReadPrec a
step = placeholder

reset :: ReadPrec a -> ReadPrec a
reset = placeholder

get :: ReadPrec Char
get = placeholder

look :: ReadPrec String
look = placeholder

(+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(+++) = placeholder

(<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(<++) = placeholder

pfail :: ReadPrec a
pfail = placeholder

choice :: [ReadPrec a] -> ReadPrec a
choice = placeholder

readPrec_to_P :: ReadPrec a -> Int -> ReadP a
readPrec_to_P = placeholder

readP_to_Prec :: (Int -> ReadP a) -> ReadPrec a
readP_to_Prec = placeholder

readPrec_to_S :: ReadPrec a -> Int -> ReadS a
readPrec_to_S = placeholder

readS_to_Prec :: (Int -> ReadS a) -> ReadPrec a
readS_to_Prec = placeholder
