{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Eq
  ( Eq (..),
  )
where

import Data.Bool (Bool)
import Prelude ()

class Eq a where
  infix 4 ==, /=
  (==), (/=) :: a -> a -> Bool
