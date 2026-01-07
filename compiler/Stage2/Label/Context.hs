module Stage2.Label.Context where

import Stage1.Variable (QualifiedConstructorIdentifier, QualifiedVariable, VariableIdentifier)
import qualified Stage2.Index.Local as Local (Index)
import qualified Stage2.Index.Table.Local as Local (Table, (!))
import qualified Stage2.Index.Table.Term as Term (Table, (!))
import qualified Stage2.Index.Table.Type as Type (Table, (!))
import qualified Stage2.Index.Term as Term (Index)
import qualified Stage2.Index.Type as Type (Index)
import Stage2.Label.Binding.Local (LocalBinding)
import qualified Stage2.Label.Binding.Local as LocalBinding
import Stage2.Label.Binding.Term (TermBinding)
import qualified Stage2.Label.Binding.Term as TermBinding
import Stage2.Label.Binding.Type (TypeBinding)
import qualified Stage2.Label.Binding.Type as TypeBinding (name)

data Context scope = Context
  { terms :: !(Term.Table TermBinding scope),
    locals :: !(Local.Table LocalBinding scope),
    types :: !(Type.Table TypeBinding scope)
  }

(!-) :: Context scope -> Term.Index scope -> TermBinding scope
Context {terms} !- index = terms Term.! index

(!-*) :: Context scope -> Term.Index scope -> QualifiedVariable
(!-*) = (TermBinding.name .) . (!-)

(!-.) :: Context scope -> Local.Index scope -> LocalBinding scope
Context {locals} !-. index = locals Local.! index

(!-.*) :: Context scope -> Local.Index scope -> VariableIdentifier
(!-.*) = (LocalBinding.name .) . (!-.)

(!=.) :: Context scope -> Type.Index scope -> TypeBinding scope
Context {types} !=. index = types Type.! index

(!=.*) :: Context scope -> Type.Index scope -> QualifiedConstructorIdentifier
(!=.*) = (TypeBinding.name .) . (!=.)
