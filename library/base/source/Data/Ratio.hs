module Data.Ratio
  ( Rational,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

import Hazy.Prelude (Ratio, placeholder)

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
