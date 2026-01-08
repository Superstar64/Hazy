{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where

data Void

absurd :: Void -> a
absurd = error "todo"

vacuous :: (Functor f) => f Void -> f a
vacuous = error "todo"
