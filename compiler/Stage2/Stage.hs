module Stage2.Stage where

import Data.Kind (Constraint, Type)

data Stage
  = Resolve
  | Check

type Resolve = 'Resolve

type Check = 'Check

-- |
-- For constructs that are currently unsupported in Stage3
type Unsupported = Equal Resolve

type Equal :: Stage -> Stage -> Type
data Equal stage1 stage2 where
  Refl :: Equal stage stage

instance Show (Equal stage1 stage2) where
  showsPrec _ Refl = showString "Refl"

instance Eq (Equal stage1 stage2) where
  Refl == Refl = True

type IsResolve :: Stage -> Constraint
class IsResolve stage where
  isResolve :: Unsupported stage

instance IsResolve 'Resolve where
  isResolve = Refl

type IsCheck :: Stage -> Constraint
class IsCheck stage where
  isCheck :: Equal Check stage

instance IsCheck 'Check where
  isCheck = Refl
