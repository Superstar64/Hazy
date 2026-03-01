module Data.Ratio
  ( Ratio,
    Rational,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

import Hazy (placeholder)

data Ratio a

type Rational = Ratio Integer

infixl 7 %

(%) :: (Integral a) => a -> a -> Ratio a
(%) = placeholder

numerator :: Ratio a -> a
numerator = placeholder

denominator :: Ratio a -> a
denominator = placeholder

approxRational :: (RealFrac a) => a -> a -> Rational
approxRational = placeholder
