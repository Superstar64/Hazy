module Stage2.Resolve.Core where

import Data.Map (Map)
import qualified Data.Map as Map
import Error (fieldNotInScope, moduleNotInScope, orderDependentUsage)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable
  ( FullQualifiers ((:..)),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier ((:=.)),
    QualifiedVariable (..),
    Qualifiers (..),
  )
import qualified Stage2.Resolve.Binding.Constructor as Constructor
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Stage2.Resolve.Bindings (Bindings)
import qualified Stage2.Resolve.Bindings as Bindings
import qualified Stage2.Resolve.Detail.Binding.Constructor as Detail.Constructor
import qualified Stage2.Resolve.Detail.Binding.Term as Detail.Term
import qualified Stage2.Resolve.Detail.Binding.Type as Detail.Type
import qualified Stage2.Resolve.Functor.Core as Functor
import Stage2.Resolve.Stability (Stability)

data Core scope = Core
  { globals :: !(Map FullQualifiers (Bindings Stability scope)),
    locals :: !(Bindings Stability scope)
  }
  deriving (Show)

instance Semigroup (Core scope) where
  Core {globals = globals1, locals = locals1}
    <> Core {globals = globals2, locals = locals2} =
      Core
        { globals = Map.unionWith (<>) globals1 globals2,
          locals = locals1 <> locals2
        }

infixl 3 !, !-, !=, !=.

Core {locals} ! _ :@ Local = locals
Core {globals} ! position :@ qualifiers :. name
  | Just bindings <- Map.lookup (qualifiers :.. name) globals = bindings
  | otherwise = moduleNotInScope position

core !- position :@ qualifiers :- name = core ! position :@ qualifiers Bindings.!- position :@ name

core != position :@ qualifiers := name = core ! position :@ qualifiers Bindings.!= position :@ name

core !=. position :@ qualifiers :=. name = core ! position :@ qualifiers Bindings.!=. position :@ name

infixl 3 !-%, !-*, !=~, !=*~, !=.*

core !-% index
  | Term.Binding {selector = Term.Selector select} <- core !- index = select
_ !-% (position :@ _) = fieldNotInScope position

(!-*) = (Term.index .) . (!-)

core !=~ index
  | binding@Constructor.Binding {unordered = False} <- core != index = binding
_ !=~ (position :@ _) = orderDependentUsage position

(!=*) = (Constructor.index .) . (!=)

(!=*~) = (Constructor.index .) . (!=~)

(!=.*) = (Type.index .) . (!=.)

infixr 5 </>

Core {globals = globals1, locals = locals1} </> Core {globals = globals2, locals = locals2} =
  Core
    { globals = Map.unionWith (Bindings.</>) globals1 globals2,
      locals = locals1 Bindings.</> locals2
    }

fromFunctor ::
  Functor.Core
    Stability
    (Detail.Term.Binding scope)
    (Detail.Constructor.Binding scope)
    (Detail.Type.Binding scope) ->
  Core scope
fromFunctor Functor.Core {globals, locals} =
  Core
    { globals = fmap Bindings.fromFunctor globals,
      locals = Bindings.fromFunctor locals
    }
