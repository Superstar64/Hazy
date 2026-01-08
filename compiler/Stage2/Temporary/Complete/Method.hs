module Stage2.Temporary.Complete.Method where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Error (duplicateMethodEntries, missingMethodEntry)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Scope (Environment ((:+)), Local)
import qualified Stage2.Temporary.Partial.Method as Partial
import qualified Stage2.Tree.Definition as Definition (merge)
import qualified Stage2.Tree.Method as Real

data Method scope = Method
  { position :: !Position,
    name :: !Variable,
    method :: Real.Method scope
  }

shrink = method

methodIndexes :: Strict.Vector (Method scope) -> Map Variable Int
methodIndexes methods = Map.fromList $ zip [name method | method <- toList methods] [0 ..]

merge :: NonEmpty (Partial.Method (Local ':+ scope)) -> Method (Local ':+ scope)
merge entries@(entry :| _) =
  Method
    { position,
      name,
      method =
        Real.Method
          { position,
            name,
            annotation,
            definition
          }
    }
  where
    position = Partial.position entry
    name = Partial.name entry
    annotation = case annotations of
      [] -> missingMethodEntry position
      [(_, annotation)] -> annotation
      annotations -> duplicateMethodEntries (map fst annotations)
    definition = case functions of
      (function : functions) -> Strict.Just (Definition.merge $ function :| functions)
      [] -> Strict.Nothing

    annotations = mapMaybe annotation (toList entries)
      where
        annotation = \case
          Partial.Annotation {position, annotation} -> Just (position, annotation)
          _ -> Nothing
    functions = mapMaybe function (toList entries)
      where
        function = \case
          Partial.Function {function} -> Just function
          _ -> Nothing
