module Stage2.Resolve.Context
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
import Stage1.Extensions (Extensions)
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable
  ( FullQualifiers (..),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier (..),
    QualifiedVariable (..),
    Qualifiers (..),
    VariableIdentifier,
  )
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Bindings (Bindings)
import Stage2.Resolve.Canonical (Canonical)
import qualified Stage2.Resolve.Canonical as Canonical
import Stage2.Resolve.Core (Core (Core))
import qualified Stage2.Resolve.Core as Core
import Stage2.Resolve.Stability (Stability)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Local)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.TypePattern (TypePattern)
import qualified Stage2.Tree.TypePattern as TypePattern
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

augmentLocalTypes :: Strict.Vector (TypePattern position) -> Context scope -> Context (Scope.Local ':+ scope)
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

(!=) = (Core.!=) . core

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
