module Stage2.Tree.ExpressionField where

import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Error (mismatchSelectors, unneededFieldQualification)
import Stage1.Position (Position)
import qualified Stage1.Tree.ExpressionField as Stage1 (Field (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (QualifiedVariable (..), Qualifiers (..))
import qualified Stage2.Index.Constructor as Constructor (Index (..))
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Context (Context (..), (!-%), (!-*))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression, variablex)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)

data Field scope
  = Field
  { index :: !Int,
    expression :: !(Expression scope)
  }
  deriving (Show)

instance Shift Field where
  shift = shiftDefault

instance Shift.Functor Field where
  map category (Field pick record) =
    Field pick (Shift.map category record)

resolve ::
  Context scope ->
  Constructor.Binding scope ->
  Stage1.Field Position ->
  Field scope
resolve context binding field = case field of
  Stage1.Field {Stage1.variable, Stage1.field} -> make variable (Expression.resolve context field)
  Stage1.Pun {Stage1.variable = variable@(position :@ _ :- localName)} ->
    let index = context !-* position :@ Local :- localName
     in make variable (variablex position index)
  where
    make name@(position :@ (path :- root)) expression = case binding of
      Constructor.Binding
        { Constructor.index = Constructor.Index typex _,
          Constructor.selections,
          Constructor.fields,
          Constructor.fielded
        }
          | fielded,
            Just index <- Map.lookup root fields -> case path of
              _ :. _ -> unneededFieldQualification position
              Local -> Field index expression
          | Selector.Index typex' selector <- context !-% name,
            typex == typex',
            Strict.Just redirect <- selections Strict.Vector.! selector ->
              Field redirect expression
          | otherwise -> mismatchSelectors position
