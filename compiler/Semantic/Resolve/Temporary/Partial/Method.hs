{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Temporary.Partial.Method where

import Data.Foldable (toList)
import Error (patternInMethod)
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context (..))
import Semantic.Resolve.Go.Scheme as Scheme (resolve)
import qualified Semantic.Resolve.Temporary.Complete.Definition as Complete
import Semantic.Stage (Resolve)
import Semantic.Tree.Function (Function)
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import qualified Syntax.Tree.ClassDeclaration as Syntax (ClassDeclaration (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (Variable)

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

resolve :: Context scope -> Syntax.ClassDeclaration Position -> [(Variable, Method scope)]
resolve context' entry = case entry of
  Syntax.Annotation {termNames, annotation} -> do
    position :@ name <- toList termNames
    annotation <- pure $ Scheme.resolve context' annotation
    pure (name, Annotation {position, name, annotation})
  Syntax.Definition {startPosition, leftHandSide, rightHandSide} ->
    case Complete.resolve (patternInMethod startPosition) context' leftHandSide rightHandSide of
      Complete.Definition position name function -> [(name, Function {position, name, function})]
  Syntax.Infix {} -> []
