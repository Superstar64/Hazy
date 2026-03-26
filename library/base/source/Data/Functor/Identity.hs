module Data.Functor.Identity (Identity (..)) where

import Hazy.Prelude (placeholder)

newtype Identity a = Identity
  { runIdentity :: a
  }
