module Semantic.Resolve.Go.ExpressionField where

import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Error (mismatchSelectors, unneededFieldQualification)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Selector as Selector
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import Semantic.Resolve.Context (Context (..), (!-%), (!-*))
import qualified Semantic.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Expression as Expression (resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Expression (callHead_)
import Semantic.Tree.ExpressionField (Field (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.ExpressionField as Syntax (Field (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (QualifiedVariable (..), Qualifiers (..))

resolve ::
  Context scope ->
  Constructor.Binding scope ->
  Syntax.Field Position ->
  Field Normal Resolve scope
resolve context binding field = case field of
  Syntax.Field {variable, field} -> make variable (Expression.resolve context field)
  Syntax.Pun {variable = variable@(position :@ _ :- localName)} ->
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
