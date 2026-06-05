module Semantic.Resolve.Context
  ( Context (..),
    empty,
    core,
    updateCore,
    augmentLocalTypes,
    (!),
    (!-),
    (!-*),
    (!-%),
    (!=),
    (!=*),
    (!=~),
    (!=*~),
    (!=.),
    (!=.*),
    (!$),
    (</>),
  )
where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict
import Error
  ( typeNotInScope,
  )
import qualified Semantic.Index.Constructor as Constructor (Index)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type3 as Type3
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding)
import Semantic.Resolve.Bindings (Bindings)
import Semantic.Resolve.Canonical (Canonical)
import qualified Semantic.Resolve.Canonical as Canonical
import Semantic.Resolve.Core (Core (Core))
import qualified Semantic.Resolve.Core as Core
import Semantic.Resolve.Stability (Stability)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Local)
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Resolve)
import Semantic.Tree.TypePattern (TypePattern)
import qualified Semantic.Tree.TypePattern as TypePattern
import Syntax.Extensions (Extensions)
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable
  ( FullQualifiers (..),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier (..),
    QualifiedVariable (..),
    Qualifiers (..),
    VariableIdentifier,
  )
import Prelude hiding (Either (Left, Right))

data Context scope = Context
  { canonical :: !(Canonical scope),
    globals :: !(Map FullQualifiers (Bindings Stability scope)),
    locals :: !(Bindings Stability scope),
    localTypes :: !(Map VariableIdentifier (Local.Index scope)),
    extensions :: !Extensions
  }

core :: Context scope -> Core scope
core Context {globals, locals} = Core {globals, locals}

updateCore Core {globals, locals} context = context {globals, locals}

instance Shift Context where
  shift = shiftDefault

instance Shift.Functor Context where
  map category Context {canonical, globals, locals, localTypes, extensions} =
    Context
      { canonical = Shift.map category canonical,
        globals = Map.map (Shift.map category) globals,
        locals = Shift.map category locals,
        localTypes = Map.map (Shift.map category) localTypes,
        extensions
      }

empty :: Extensions -> Context scope
empty extensions =
  Context
    { canonical = Canonical.empty,
      globals = Map.empty,
      locals = mempty,
      localTypes = Map.empty,
      extensions
    }

augmentLocalTypes ::
  Strict.Vector (TypePattern position Resolve scope') ->
  Context scope ->
  Context (Scope.Local ':+ scope)
augmentLocalTypes parameters context
  | context@Context {localTypes} <- shift context =
      context
        { localTypes = types <> localTypes
        }
  where
    names = TypePattern.name <$> parameters
    types = Map.fromList $ zip (toList names) [Local.Local i | i <- [0 ..]]

infixl 3 !, !-, !-%, !-*, !=., !=.*, !=, !=~, !=*, !=*~, !$

(!) :: Context scope -> Marked Qualifiers Position -> Bindings Stability scope
(!) = (Core.!) . core

(!-) = (Core.!-) . core

(!-%) :: Context scope -> Marked QualifiedVariable Position -> Selector.Index scope
(!-%) = (Core.!-%) . core

(!-*) :: Context scope -> Marked QualifiedVariable Position -> Term2.Index scope
(!-*) = (Core.!-*) . core

(!=.) = (Core.!=.) . core

(!=.*) :: Context scope -> Marked QualifiedConstructorIdentifier Position -> Type3.Index scope
(!=.*) = (Core.!=.*) . core

(!=) :: Context scope -> Marked QualifiedConstructor Position -> Constructor.Binding scope
(!=) = (Core.!=) . core

(!=~) :: Context scope -> Marked QualifiedConstructor Position -> Constructor.Binding scope
(!=~) = (Core.!=~) . core

(!=*) :: Context scope -> Marked QualifiedConstructor Position -> Constructor.Index scope
(!=*) = (Core.!=*) . core

(!=*~) :: Context scope -> Marked QualifiedConstructor Position -> Constructor.Index scope
(!=*~) = (Core.!=*~) . core

(!$) :: Context scope -> Marked VariableIdentifier Position -> Local.Index scope
(!$) Context {localTypes} (position :@ name)
  | Just index <- Map.lookup name localTypes = index
  | otherwise = typeNotInScope position

infixr 5 </>

(</>) :: Core scope -> Context scope -> Context scope
left </> right =
  right
    { globals,
      locals
    }
  where
    Core {globals, locals} = left Core.</> core right
