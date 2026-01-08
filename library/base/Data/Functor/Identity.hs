{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Functor.Identity (Identity (..)) where

newtype Identity a = Identity
  { runIdentity :: a
  }
