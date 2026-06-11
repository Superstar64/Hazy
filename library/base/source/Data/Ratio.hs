module Data.Ratio
  ( Rational,
    Ratio,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

import Hazy.Prelude
  ( Ratio,
    denominator,
    numerator,
    placeholder,
    (%),
  )

approxRational :: (RealFrac a) => a -> a -> Rational
approxRational = placeholder
