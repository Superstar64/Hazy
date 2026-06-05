module Semantic.Label.Context where

import qualified Semantic.Index.Local as Local (Index)
import qualified Semantic.Index.Table.Local as Local (Table, (!))
import qualified Semantic.Index.Table.Term as Term (Table, (!))
import qualified Semantic.Index.Table.Type as Type (Table, (!))
import qualified Semantic.Index.Term as Term (Index)
import qualified Semantic.Index.Type as Type (Index)
import Semantic.Label.Binding.Local (LocalBinding)
import qualified Semantic.Label.Binding.Local as LocalBinding
import Semantic.Label.Binding.Term (TermBinding (..))
import Semantic.Label.Binding.Type (TypeBinding)
import qualified Semantic.Label.Binding.Type as TypeBinding (name)
import Syntax.Variable (QualifiedConstructorIdentifier, QualifiedVariable, VariableIdentifier)

data Context scope = Context
  { terms :: !(Term.Table TermBinding scope),
    locals :: !(Local.Table LocalBinding scope),
    types :: !(Type.Table TypeBinding scope)
  }

(!-) :: Context scope -> Term.Index scope -> TermBinding scope
Context {terms} !- index = terms Term.! index

(!-*) :: Context scope -> Term.Index scope -> QualifiedVariable
context !-* index = case context !- index of
  TermBinding {name} -> name
  _ -> error "bad label term binding"

(!-.) :: Context scope -> Local.Index scope -> LocalBinding scope
Context {locals} !-. index = locals Local.! index

(!-.*) :: Context scope -> Local.Index scope -> VariableIdentifier
(!-.*) = (LocalBinding.name .) . (!-.)

(!=.) :: Context scope -> Type.Index scope -> TypeBinding scope
Context {types} !=. index = types Type.! index

(!=.*) :: Context scope -> Type.Index scope -> QualifiedConstructorIdentifier
(!=.*) = (TypeBinding.name .) . (!=.)
