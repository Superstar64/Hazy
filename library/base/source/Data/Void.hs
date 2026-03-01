module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where

import Hazy (placeholder)

data Void

absurd :: Void -> a
absurd = placeholder

vacuous :: (Functor f) => f Void -> f a
vacuous = placeholder
