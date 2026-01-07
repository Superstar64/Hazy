module Stage3.Simple.SchemeOver where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Error (Position)
import Stage1.Variable (VariableIdentifier)
import Stage2.Scope (Environment ((:+)), Local)
import Stage3.Check.Context (Context)
import Stage3.Simple.Constraint (Constraint)
import Stage3.Simple.Type (Type)

augmentNamed ::
  (Int -> VariableIdentifier) ->
  Position ->
  Strict.Vector (Type scope) ->
  Strict.Vector (Constraint scope) ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
