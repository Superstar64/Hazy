{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Ratio
  ( Ratio,
    Rational,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

data Ratio a

type Rational = Ratio Integer

infixl 7 %

(%) :: (Integral a) => a -> a -> Ratio a
(%) = error "todo"

numerator :: Ratio a -> a
numerator = error "todo"

denominator :: Ratio a -> a
denominator = error "todo"

approxRational :: (RealFrac a) => a -> a -> Rational
approxRational = error "todo"
