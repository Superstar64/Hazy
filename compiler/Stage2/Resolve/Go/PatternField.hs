module Stage2.Resolve.Go.PatternField where

import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Error (mismatchSelectors, unneededFieldQualification)
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.PatternField as Stage1 (Field (..))
import Stage1.Variable (QualifiedVariable (..), Qualifiers (..))
import qualified Stage2.Index.Constructor as Constructor (Index (..))
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Context (Context (..), (!-%))
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Pattern as Pattern (resolve)
import Stage2.Stage (Resolve)
import qualified Stage2.Tree.Pattern as Pattern (variable)
import Stage2.Tree.PatternField (Field (..))

resolve ::
  Context scope ->
  Constructor.Binding scope ->
  Stage1.Field Position ->
  Field Resolve scope
resolve context binding field = case field of
  Stage1.Field {variable, patternx} ->
    make variable (Pattern.resolve context patternx)
  Stage1.Pun {variable = variable@(position :@ _ :- localName)} ->
    make variable (Pattern.variable position localName)
  where
    make (position :@ name@(path :- root)) patternx = case binding of
      Constructor.Binding
        { index = Constructor.Index typex _,
          selections,
          fields,
          fielded
        }
          | fielded,
            Just index <- Map.lookup root fields -> case path of
              _ :. _ -> unneededFieldQualification position
              Local -> Field index patternx
          | Selector.Index typex' selector <- context !-% (position :@ name),
            typex == typex',
            Strict.Just redirect <- selections Strict.Vector.! selector ->
              Field redirect patternx
          | otherwise -> mismatchSelectors position
