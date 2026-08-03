module Semantic.Unify.Constraints where

import Core.Tree.Constraints (ConstraintsF (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

type Constraints s scope = ConstraintsF (Logical s scope) scope
