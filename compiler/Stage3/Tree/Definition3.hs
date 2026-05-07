module Stage3.Tree.Definition3 where

import Stage2.Tree.Definition3 (Info)
import Stage3.Tree.Definition2 (Definition2)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))

data Definition3 mark scope where
  (::@) :: !(Info source) -> !(Simple.SchemeOver (Definition2 source mark) scope) -> Definition3 mark scope

infixr 5 ::@

instance Show (Definition3 mark scope) where
  showsPrec d (info ::@ definition) =
    showParen (d > 5) $
      showsPrec 6 info . showString " ::@ " . showsPrec 6 definition
