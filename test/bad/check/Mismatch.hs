{-# LANGUAGE NoImplicitPrelude #-}

module BadExample2 where

import Hazy (Bool (False, True))

data Data a b = Data a (a -> b)

run :: Bool -> ()
run False = ()
run True = ()

value :: Data Bool ()
value = Data () run
