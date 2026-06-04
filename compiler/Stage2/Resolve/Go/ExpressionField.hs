module Stage2.Resolve.Go.ExpressionField where

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
import Stage2.Layout (Normal)
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Context (Context (..), (!-%), (!-*))
import qualified Stage2.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Expression as Expression (resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Expression (callHead_)
import Stage2.Tree.ExpressionField (Field (..))

resolve ::
  Context scope ->
  Constructor.Binding scope ->
  Stage1.Field Position ->
  Field Normal Resolve scope
resolve context binding field = case field of
  Stage1.Field {variable, field} -> make variable (Expression.resolve context field)
  Stage1.Pun {variable = variable@(position :@ _ :- localName)} ->
    let index = context !-* position :@ Local :- localName
     in make variable $ callHead_ (CallHead.resolveVariable position index)
  where
    make name@(position :@ (path :- root)) expression = case binding of
      Constructor.Binding
        { index = Constructor.Index typex _,
          selections,
          fields,
          fielded
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
