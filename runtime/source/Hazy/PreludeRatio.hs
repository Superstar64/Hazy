module Hazy.PreludeRatio where

import Hazy.Helper
import Hazy.Prelude

type Rational = Ratio Integer

(%) :: (Integral a) => a -> a -> Ratio a
numerator, denominator :: (Integral a) => Ratio a -> a
x % y = ratio (reduce (x * signum y) (abs y))

numerator (x :% _) = x

denominator (_ :% y) = y
