module Stage3.Tree.Definition4 where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import qualified Stage2.Tree.Definition2 as Mark
import Stage3.Tree.Definition3 (Definition3)
import Stage3.Tree.Scheme (Scheme)

data Definition4 scope where
  (:::) :: !(Annotation mark scope) -> !(Definition3 mark scope) -> Definition4 scope

instance Show (Definition4 scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 5) $
      showsPrec 6 annotation . showString " ::: " . showsPrec 6 definition

infixr 5 :::

data Annotation mark scope where
  Annotated :: !(Scheme Position Check scope) -> Annotation Mark.Annotated scope
  Inferred :: Annotation Mark.Inferred scope

instance Show (Annotation mark scope) where
  showsPrec d = \case
    Annotated scheme -> showParen (d > 10) $ showString "Annotated " . showsPrec 11 scheme
    Inferred -> showString "Inferred"
