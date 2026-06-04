{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.Method where

import Data.Foldable (toList)
import Error (patternInMethod)
import Stage1.Position (Position)
import qualified Stage1.Tree.ClassDeclaration as Stage1 (ClassDeclaration (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (Variable)
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context (..))
import Stage2.Resolve.Go.Scheme as Scheme (resolve)
import Stage2.Stage (Resolve)
import qualified Stage2.Temporary.Complete.Definition as Complete
import Stage2.Tree.Function (Function)
import Stage2.Tree.Scheme (Scheme)

data Method scope
  = Annotation
      { position :: !Position,
        name :: !Variable,
        annotation :: Scheme Position Resolve scope
      }
  | Function
      { position :: !Position,
        name :: !Variable,
        function :: Function Normal Resolve scope
      }

resolve :: Context scope -> Stage1.ClassDeclaration Position -> [(Variable, Method scope)]
resolve context' entry = case entry of
  Stage1.Annotation {termNames, annotation} -> do
    position :@ name <- toList termNames
    annotation <- pure $ Scheme.resolve context' annotation
    pure (name, Annotation {position, name, annotation})
  Stage1.Definition {startPosition, leftHandSide, rightHandSide} ->
    case Complete.resolve (patternInMethod startPosition) context' leftHandSide rightHandSide of
      Complete.Definition position name function -> [(name, Function {position, name, function})]
  Stage1.Infix {} -> []
