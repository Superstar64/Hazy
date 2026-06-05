module Stage2.Check.Simple.Constructor where

import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage2.Check.ConstructorInstance as ConstructorInstance
import qualified Stage2.Check.EntryInstance as EntryInstance
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import Stage4.Tree.Constructor (Constructor (..))

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
