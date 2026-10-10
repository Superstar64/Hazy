module Semantic.Check.Simple.Constructor where

import Core.Instanciate (Store (..))
import Core.Tree.Constructor (Constructor, ConstructorF (..))
import qualified Data.Vector.Strict as Strict
import Semantic.Check.ConstructorInstance (ConstructorInstance)
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

instanciate ::
  Position ->
  Brand ->
  Strict.Vector (Unify.Type s scope) ->
  Constructor (Local ':+ scope) ->
  ConstructorInstance s scope
instanciate position brand fresh Constructor {name, syntax, entries} =
  Constructor
    { brand = Store brand,
      name,
      syntax,
      entries = EntryInstance.instanciate position fresh <$> entries
    }
