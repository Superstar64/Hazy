{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TermDeclaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Annotation (Annotation)
import Stage2.Tree.Definition2 (Definition2)
import Prelude hiding (Either (Left, Right))

data TermDeclaration scope
  = TermDeclaration
  { position :: !Position,
    name :: !Variable,
    fixity :: !Fixity,
    declaration :: !(TermDeclaration' scope)
  }
  deriving (Show)

instance Shift TermDeclaration where
  shift = shiftDefault

instance Shift.Functor TermDeclaration where
  map category = \case
    TermDeclaration {position, name, fixity, declaration} ->
      TermDeclaration
        { position,
          name,
          fixity,
          declaration = Shift.map category declaration
        }

data TermDeclaration' scope where
  (:::) :: !(Annotation mark scope) -> !(Definition2 mark scope) -> TermDeclaration' scope

infix 9 :::

instance Show (TermDeclaration' scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 9) $
      foldr
        (.)
        id
        [ showsPrec 10 annotation,
          showString " ::: ",
          showsPrec 10 definition
        ]

instance Shift TermDeclaration' where
  shift = shiftDefault

instance Shift.Functor TermDeclaration' where
  map category (annotation ::: definition) =
    Shift.map category annotation ::: Shift.map category definition

labelBinding :: Qualifiers -> TermDeclaration scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}
