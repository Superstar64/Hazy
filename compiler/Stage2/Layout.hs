module Stage2.Layout where

import Data.Kind (Type)

data Layout
  = Normal
  | Group

type Normal = 'Normal

type Group = 'Group

type Equal :: Layout -> Layout -> Type
data Equal layout1 layout where
  Refl :: Equal layout layout

class IsNormal layout where
  isNormal :: Equal Normal layout

instance IsNormal 'Normal where
  isNormal = Refl
