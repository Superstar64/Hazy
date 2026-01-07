module ZeroField where

import qualified Hazy
import Prelude ()

data Zero = Zero

-- test user defined and builtin num

class Num a where
  (+), (-), (*) :: a -> a -> a
  infixl 6 +, -
  infixl 7 *
  negate, abs, signum :: a -> a
  fromInteger :: Hazy.Integer -> a

instance Num Zero where
  Zero + Zero = Zero
  Zero - Zero = Zero
  Zero * Zero = Zero
  negate Zero = Zero
  abs Zero = Zero
  signum Zero = Zero
  fromInteger _ = Zero

instance Hazy.Num Zero where
  Zero + Zero = Zero
  Zero - Zero = Zero
  Zero * Zero = Zero
  negate Zero = Zero
  abs Zero = Zero
  signum Zero = Zero
  fromInteger _ = Zero

zeros :: [Zero]
zeros = [1, 2, 3]
