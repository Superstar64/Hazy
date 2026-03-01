module Data.Functor.Identity (Identity (..)) where

import Hazy (placeholder)

newtype Identity a = Identity
  { runIdentity :: a
  }
