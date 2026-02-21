-- |
-- Unification public api
module Stage3.Unify
  ( Type,
    Scheme (..),
    SchemeOver,
    Constraint,
    Evidence,
    Instanciation,
    scheme,
    schemeOver,
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
    function,
    variable',
    call',
    super,
    fresh,
    unify,
    constrain,
    Zonk (..),
    Zonker,
    Generalizable (..),
    Generalize (..),
    generalizeOver,
    generalize,
    solve,
    solveEvidence,
    solveInstanciation,
    Solve (..),
    solveSchemeOver,
    instanciate,
    Functor (..),
    Category,
    MapScheme (..),
    mapScheme,
  )
where

import Control.Monad.ST (ST)
import qualified Data.Kind
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Index.Evidence as Evidence (Index (..))
import Stage3.Unify.Class
  ( Category,
    Functor (..),
    Generalizable (collect),
    Zonk (..),
    Zonker (..),
  )
import Stage3.Unify.Constraint (Constraint)
import qualified Stage3.Unify.Constraint as Constraint
import Stage3.Unify.Evidence (Evidence)
import qualified Stage3.Unify.Evidence as Evidence (Evidence (..), solve)
import Stage3.Unify.Instanciation (Instanciation (..))
import qualified Stage3.Unify.Instanciation as Instanciation
import Stage3.Unify.SchemeOver (Generalize (..), SchemeOver (..), Solve (..), generalizeOver, instanciateOver)
import qualified Stage3.Unify.SchemeOver as SchemeOver
import Stage3.Unify.Type
  ( Type (..),
    constrain,
    fresh,
    solve,
    unify,
  )
import qualified Stage4.Tree.Constraint as Simple (Constraint)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)
import Prelude hiding (Functor, head)

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

instance Shift (Scheme s) where
  shift (Scheme scheme) = Scheme (shift scheme)

type MapScheme ::
  (Data.Kind.Type -> Environment -> Data.Kind.Type) ->
  (Data.Kind.Type -> Environment -> Data.Kind.Type) ->
  Data.Kind.Type
newtype MapScheme typex typex' = MapScheme (forall s scope. typex s scope -> typex' s scope)

mapScheme :: MapScheme typex typex' -> SchemeOver typex s scope -> SchemeOver typex' s scope
mapScheme (MapScheme map) SchemeOver {parameters, constraints, result} =
  SchemeOver
    { parameters,
      constraints,
      result = map result
    }

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

infixr 9 `function`

function :: Type s scope -> Type s scope -> Type s scope
function = Function

scheme ::
  Strict.Vector.Vector (Type s scope) ->
  Strict.Vector.Vector (Constraint s scope) ->
  Type s (Scope.Local ':+ scope) ->
  Scheme s scope
scheme parameters constraints result =
  Scheme
    (schemeOver parameters constraints result)

-- todo, this function isn't safe
schemeOver ::
  Strict.Vector (Type s scope) ->
  Strict.Vector (Constraint s scope) ->
  typex s (Scope.Local ':+ scope) ->
  SchemeOver typex s scope
schemeOver parameters constraints result =
  SchemeOver
    { parameters,
      constraints,
      result
    }

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
      constraints = Strict.Vector.empty,
      result = shift result
    }

monoScheme :: Type s scope -> Scheme s scope
monoScheme = Scheme . mono

variable' :: Evidence.Index scope -> Evidence s scope
variable' = Evidence.Variable

call' :: Evidence s scope -> Strict.Vector (Evidence s scope) -> Evidence s scope
call' = Evidence.Call

super :: Evidence s scope -> Int -> Evidence s scope
super = Evidence.Super

instanciate :: Context s scope -> Position -> Scheme s scope -> ST s (Type s scope, Instanciation s scope)
instanciate context position Scheme {runScheme} =
  instanciateOver context position runScheme

generalize :: Context s scopes -> Generalize Type s scopes -> ST s (Scheme s scopes)
generalize context = fmap Scheme . generalizeOver context

solveEvidence :: Position -> Evidence s scope -> ST s (Simple.Evidence scope)
solveEvidence = Evidence.solve

solveInstanciation :: Position -> Instanciation s scope -> ST s (Simple.Instanciation scope)
solveInstanciation = Instanciation.solve

solveConstraint :: Position -> Constraint s scope -> ST s (Simple.Constraint scope)
solveConstraint = Constraint.solve

solveSchemeOver ::
  Solve source target ->
  Position ->
  SchemeOver source s scope ->
  ST s (Simple.SchemeOver target scope)
solveSchemeOver = SchemeOver.solve
