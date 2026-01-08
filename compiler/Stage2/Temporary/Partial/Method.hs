{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.Method where

import Data.Foldable (toList)
import Error (patternInMethod)
import Stage1.Position (Position)
import qualified Stage1.Tree.ClassDeclaration as Stage1 (ClassDeclaration (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (Variable)
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Temporary.Partial.Definition as Partial
import Stage2.Tree.Function (Function)
import Stage2.Tree.Scheme (Scheme)
import Stage2.Tree.Scheme as Scheme (resolve)

data Method scope
  = Annotation
      { position :: !Position,
        name :: !Variable,
        annotation :: Scheme Position scope
      }
  | Function
      { position :: !Position,
        name :: !Variable,
        function :: Function scope
      }

resolve :: Context scope -> Stage1.ClassDeclaration Position -> [(Variable, Method scope)]
resolve context' entry = case entry of
  Stage1.Annotation {termNames, annotation} -> do
    position :@ name <- toList termNames
    annotation <- pure $ Scheme.resolve context' annotation
    pure (name, Annotation {position, name, annotation})
  Stage1.Definition {startPosition, leftHandSide, rightHandSide} ->
    case Partial.resolve (patternInMethod startPosition) context' leftHandSide rightHandSide of
      Partial.Definition position name function -> [(name, Function {position, name, function})]
  Stage1.Infix {} -> []
