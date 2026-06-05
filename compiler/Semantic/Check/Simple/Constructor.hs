module Semantic.Check.Simple.Constructor where

import Core.Tree.Constructor (Constructor (..))
import qualified Data.Vector.Strict as Strict
import Semantic.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Scope (Environment ((:+)), Local)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

instanciate ::
  Position ->
  Brand ->
  Strict.Vector (Unify.Type s scope) ->
  Constructor (Local ':+ scope) ->
  ConstructorInstance s scope
instanciate position brand fresh Constructor {entries} =
  ConstructorInstance
    { entries = EntryInstance.instanciate position fresh <$> entries,
      brand
    }
