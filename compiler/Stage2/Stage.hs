module Stage2.Stage where

import Data.Kind (Constraint)

data Stage
  = Resolve
  | Check

type Resolve = 'Resolve

type Check = 'Check

-- |
-- For constructs that are currently unsupported in Stage3
data Unsupported stage where
  Placeholder :: Unsupported Resolve

instance Show (Unsupported stage) where
  showsPrec _ Placeholder = showString "Placeholder"

instance Eq (Unsupported stage) where
  Placeholder == Placeholder = True

type Exclusive :: Stage -> Constraint
class Exclusive stage where
  exclusive :: Unsupported stage

instance Exclusive 'Resolve where
  exclusive = Placeholder
