{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read (ReadS)
import Prelude (error)

data ReadPrec a

type Prec = Int

minPrec :: Prec
minPrec = error "todo"

lift :: ReadP a -> ReadPrec a
lift = error "todo"

prec :: Prec -> ReadPrec a -> ReadPrec a
prec = error "todo"

step :: ReadPrec a -> ReadPrec a
step = error "todo"

reset :: ReadPrec a -> ReadPrec a
reset = error "todo"

get :: ReadPrec Char
get = error "todo"

look :: ReadPrec String
look = error "todo"

(+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(+++) = error "todo"

(<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(<++) = error "todo"

pfail :: ReadPrec a
pfail = error "todo"

choice :: [ReadPrec a] -> ReadPrec a
choice = error "todo"

readPrec_to_P :: ReadPrec a -> Int -> ReadP a
readPrec_to_P = error "todo"

readP_to_Prec :: (Int -> ReadP a) -> ReadPrec a
readP_to_Prec = error "todo"

readPrec_to_S :: ReadPrec a -> Int -> ReadS a
readPrec_to_S = error "todo"

readS_to_Prec :: (Int -> ReadS a) -> ReadPrec a
readS_to_Prec = error "todo"
