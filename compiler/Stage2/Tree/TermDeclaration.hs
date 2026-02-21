{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.TermDeclaration where

import qualified Data.Strict.Maybe as Strict
import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (QualifiedVariable ((:-)), Qualifiers, Variable)
import qualified Stage2.Index.Term as Term (Bound)
import qualified Stage2.Label.Binding.Term as Label
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.Scheme (Scheme)
import Prelude hiding (Either (Left, Right))

data TermDeclaration scope
  = Manual
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition :: Definition (Local ':+ scope),
        annotation :: !(Scheme Position scope)
      }
  | Auto
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definitionAuto :: !(Definition scope)
      }
  | Share
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        shareIndex :: !Int,
        bound :: !Term.Bound,
        patternx :: Pattern scope,
        annotationShare :: !(Strict.Maybe (Scheme Position scope))
      }
  deriving (Show)

instance Shift TermDeclaration where
  shift = shiftDefault

instance Shift.Functor TermDeclaration where
  map category = \case
    Manual {position, name, fixity, definition, annotation} ->
      Manual
        { position,
          name,
          fixity,
          definition = Shift.map (Shift.Over category) definition,
          annotation = Shift.map category annotation
        }
    Auto {position, name, fixity, definitionAuto} ->
      Auto
        { position,
          name,
          fixity,
          definitionAuto = Shift.map category definitionAuto
        }
    Share {position, name, fixity, shareIndex, bound, patternx, annotationShare} ->
      Share
        { position,
          name,
          fixity,
          shareIndex,
          bound,
          patternx = Shift.map category patternx,
          annotationShare = fmap (Shift.map category) annotationShare
        }

labelBinding :: Qualifiers -> TermDeclaration scope -> Label.TermBinding scope'
labelBinding path declaration = Label.TermBinding {name = path :- name declaration}
