module Data.Functor.Identity (Identity (..)) where

newtype Identity a = Identity
  { runIdentity :: a
  }
