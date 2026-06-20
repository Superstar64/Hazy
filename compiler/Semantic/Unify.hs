-- |
-- Unification public api
module Semantic.Unify
  ( Type,
    Scheme (..),
    SchemeOver,
    Constraints,
    Constraint,
    Evidence,
    Instanciation,
    scheme,
    schemeOver,
    constraints,
    none,
    constraintx,
    mono,
    monoScheme,
    variable,
    constructor,
    call,
    index,
    lifted,
    arrow,
    list,
    listWith,
    tuple,
    bool,
    char,
    typex,
    kind,
    typeWith,
    small,
    large,
    universe,
    constraint,
    levity,
    function,
    variable',
    super,
    instanciation,
    monoInstanciation,
    fresh,
    mark,
    unify,
    constrain,
    Zonk (..),
    Zonker,
    Generalizable (..),
    Generalize (..),
    Body ((:::)),
    generalizeBody,
    Solve,
    liftST,
    runSolve,
    solve,
    solveEvidence,
    solveInstanciation,
    SolveScheme (..),
    solveSchemeOver,
    solveScheme,
    instanciate,
    Functor (..),
    Category,
    MapScheme (..),
    mapScheme,
  )
where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraint as Simple (Constraint)
import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import qualified Core.Tree.Scheme as Simple (Scheme (..))
import qualified Core.Tree.SchemeOver as Simple (SchemeOver)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class
  ( Category,
    Functor (..),
    Generalizable (collect),
    Solve (..),
    Zonk (..),
    Zonker (..),
  )
import Semantic.Unify.Constraint (Constraint)
import qualified Semantic.Unify.Constraint as Constraint
import Semantic.Unify.Constraints (Constraints (..))
import qualified Semantic.Unify.Constraints as Constraints
import Semantic.Unify.Evidence (Evidence)
import qualified Semantic.Unify.Evidence as Evidence (Evidence (..), solve)
import Semantic.Unify.Instanciation (Instanciation (..))
import qualified Semantic.Unify.Instanciation as Instanciation
import Semantic.Unify.SchemeOver
  ( Body ((:::)),
    Generalize (..),
    MapScheme (..),
    SchemeOver (SchemeOver),
    SolveScheme (..),
    generalizeBody,
    instanciateOver,
    mapScheme,
  )
import qualified Semantic.Unify.SchemeOver as SchemeOver
import Semantic.Unify.Type
  ( Type (..),
    constrain,
    fresh,
    mark,
    solve,
    unify,
  )
import Syntax.Position (Position)
import Prelude hiding (Functor, head)

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

instance Shift (Scheme s) where
  shift (Scheme scheme) = Scheme (shift scheme)

instance Zonk Scheme where
  zonk zonker (Scheme scheme) = do
    scheme <- zonk zonker scheme
    pure (Scheme scheme)

variable :: Local.Index scope -> Type s scope
variable = Variable

constructor :: Type2.Index scope -> Type s scope
constructor = Constructor

call :: Type s scope -> Type s scope -> Type s scope
call = Call

index :: Type.Index scope -> Type s scope
index = constructor . Type2.Index

lifted :: Constructor.Index scope -> Type s scope
lifted index = constructor (Type2.Lifted index)

arrow :: Type s scope
arrow = constructor Type2.Arrow

list :: Type s scope
list = constructor Type2.List

listWith :: Type s scope -> Type s scope
listWith = call list

tuple :: Int -> Type s scope
tuple size = constructor (Type2.Tuple size)

bool :: Type s scope
bool = constructor Type2.Bool

char :: Type s scope
char = constructor Type2.Char

typex :: Type s scope
typex = Type small

kind :: Type s scope
kind = Type large

typeWith :: Type s scopes -> Type s scopes
typeWith = Type

small :: Type s scopes
small = Small

large :: Type s scopes
large = Large

universe :: Type s scopes
universe = Universe

constraint :: Type s scope
constraint = Constraint

levity :: Type s scope
levity = Levity

infixr 0 `function`

function :: Type s scope -> Type s scope -> Type s scope
function = Function

scheme ::
  Strict.Vector.Vector (Type s scope) ->
  Constraints s scope ->
  Type s (Scope.Local ':+ scope) ->
  Scheme s scope
scheme parameters constraints result =
  Scheme
    (schemeOver parameters constraints result)

-- todo, this function isn't safe
schemeOver ::
  Strict.Vector (Type s scope) ->
  Constraints s scope ->
  typex s (Scope.Local ':+ scope) ->
  SchemeOver typex s scope
schemeOver parameters constraints result =
  SchemeOver
    { parameters,
      constraints,
      result
    }

constraints :: Strict.Vector (Constraint s scope) -> Constraints s scope
constraints = Constraints

none :: Constraints s scope
none = None

constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
constraintx classx head arguments =
  Constraint.Constraint
    { classx,
      head,
      arguments
    }

mono :: (Shift (typex s)) => typex s scope -> SchemeOver typex s scope
mono result =
  SchemeOver
    { parameters = Strict.Vector.empty,
      constraints = Constraints.None,
      result = shift result
    }

monoScheme :: Type s scope -> Scheme s scope
monoScheme = Scheme . mono

variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
variable' = Evidence.Variable

super :: Evidence s scope -> Int -> Evidence s scope
super = Evidence.Super

instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
instanciation = Instanciation

monoInstanciation :: Instanciation s scope
monoInstanciation = Instanciation.Mono

instanciate :: Context s scope -> Position -> Scheme s scope -> ST s (Type s scope, Instanciation s scope)
instanciate context position Scheme {runScheme} =
  instanciateOver context position runScheme

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a

-- todo, figure out how to make sure no unification occurs during solving
liftST :: ST s a -> Solve s a
liftST = Solve

solveEvidence :: Position -> Evidence s scope -> Solve s (Simple.Evidence scope)
solveEvidence = Evidence.solve

solveInstanciation :: Position -> Instanciation s scope -> Solve s (Simple.Instanciation scope)
solveInstanciation = Instanciation.solve

solveConstraint :: Position -> Constraint s scope -> Solve s (Simple.Constraint scope)
solveConstraint = Constraint.solve

solveScheme :: Position -> Scheme s scope -> Solve s (Simple.Scheme scope)
solveScheme position (Scheme scheme) = Simple.Scheme <$> solveSchemeOver (SolveScheme solve) position scheme

solveSchemeOver ::
  SolveScheme source target ->
  Position ->
  SchemeOver source s scope ->
  Solve s (Simple.SchemeOver target scope)
solveSchemeOver = SchemeOver.solve
