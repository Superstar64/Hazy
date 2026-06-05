module Semantic.Resolve.Go.PatternField where

import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Error (mismatchSelectors, unneededFieldQualification)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import Semantic.Resolve.Context (Context (..), (!-%))
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Pattern as Pattern (resolve)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Pattern as Pattern (variable)
import Semantic.Tree.PatternField (Field (..))
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.PatternField as Syntax (Field (..))
import Syntax.Variable (QualifiedVariable (..), Qualifiers (..))

resolve ::
  Context scope ->
  Constructor.Binding scope ->
  Syntax.Field Position ->
  Field Resolve scope
resolve context binding field = case field of
  Syntax.Field {variable, patternx} ->
    make variable (Pattern.resolve context patternx)
  Syntax.Pun {variable = variable@(position :@ _ :- localName)} ->
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
