module Data.Ratio
  ( Rational,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

import Hazy.Prelude
  ( Ratio,
    Rational,
    denominator,
    numerator,
    placeholder,
    (%),
  )
import Prelude (RealFrac)

approxRational :: (RealFrac a) => a -> a -> Rational
approxRational = placeholder
