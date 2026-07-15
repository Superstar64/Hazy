-- |
-- Unification public api
module Semantic.Unify
  ( Type,
    Forall,
    ForallOver,
    Constraints,
    Constraint,
    Evidence,
    Instanciation,
    forallx,
    constraints,
    none,
    constraintx,
    mono,
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
    SolveForall (..),
    solveForallOver,
    solveForall,
    instanciate,
    MapForall (..),
    mapForall,
    liftWith,
    liftWith',
    lift,
  )
where

import Control.Monad.ST (ST)
import Core.Substitute (Category (Substitute))
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Constraint as Constraint (ConstraintF (..))
import qualified Core.Tree.Constraint as Simple (Constraint)
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Forall as Simple (Forall, ForallOver)
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import Core.Tree.Type (TypeF (..))
import qualified Core.Tree.Type as Simple (Type)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type as Type (Index)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..))
import qualified Semantic.Shift as Shift
import Semantic.Unify.Class
  ( Generalizable (collect),
    Solve (..),
    Zonk (..),
    Zonker (..),
  )
import Semantic.Unify.Constraint (Constraint (..))
import qualified Semantic.Unify.Constraint as Constraint (solve)
import Semantic.Unify.Constraints (Constraints (..))
import Semantic.Unify.Evidence (Evidence (..))
import qualified Semantic.Unify.Evidence as Evidence (solve)
import Semantic.Unify.Forall
  ( Body ((:::)),
    Forall,
    ForallOver (ForallOver),
    Generalize (..),
    MapForall (..),
    SolveForall (..),
    generalizeBody,
    instanciateOver,
    mapForall,
  )
import qualified Semantic.Unify.Forall
import qualified Semantic.Unify.Forall as Forall
import Semantic.Unify.Instanciation (Instanciation (..))
import qualified Semantic.Unify.Instanciation as Instanciation
import Semantic.Unify.Type (Type (..))
import qualified Semantic.Unify.Type as Type (constrain, fresh, liftWith, mark, solve, unify)
import Syntax.Position (Position)
import Prelude hiding (Functor, head)

variable :: Local.Index scope -> Type s scope
variable = Typex . Variable

constructor :: Type2.Index scope -> Type s scope
constructor = Typex . Constructor

call :: Type s scope -> Type s scope -> Type s scope
call (Typex argument) (Typex result) = Typex (Call argument result)

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
typex = Typex $ Type Small

kind :: Type s scope
kind = Typex $ Type Large

typeWith :: Type s scopes -> Type s scopes
typeWith (Typex universe) = Typex $ Type universe

small :: Type s scopes
small = Typex Small

large :: Type s scopes
large = Typex Large

universe :: Type s scopes
universe = Typex Universe

constraint :: Type s scope
constraint = Typex Constraint

levity :: Type s scope
levity = Typex Levity

infixr 0 `function`

function :: Type s scope -> Type s scope -> Type s scope
function (Typex parameter) (Typex result) = Typex $ Function parameter result

-- todo, this function isn't safe
forallx ::
  Strict.Vector (Type s scope) ->
  Constraints s scope ->
  typex s (Scope.Local ':+ scope) ->
  ForallOver typex s scope
forallx parameters constraints result =
  ForallOver
    { parameters = fmap runTypex parameters,
      constraints = runConstraintsx constraints,
      result
    }

constraints :: Strict.Vector (Constraint s scope) -> Constraints s scope
constraints constraints = Constraintsx $ Constraints $ fmap runConstraintx constraints

none :: Constraints s scope
none = Constraintsx None

constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
constraintx classx head arguments =
  Constraintx
    Constraint.Constraint
      { classx,
        head,
        arguments = fmap runTypex arguments
      }

mono :: (Shift (typex s)) => typex s scope -> ForallOver typex s scope
mono result =
  ForallOver
    { parameters = Strict.Vector.empty,
      constraints = None,
      result = shift result
    }

variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
variable' variable (Instanciationx instanciation) = Evidencex $ Evidence.Variable {variable, instanciation}

super :: Evidence s scope -> Int -> Evidence s scope
super (Evidencex base) index =
  Evidencex
    Evidence.Super
      { base,
        index
      }

instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
instanciation instanciation = Instanciationx $ Instanciation $ fmap runEvidencex instanciation

monoInstanciation :: Instanciation s scope
monoInstanciation = Instanciationx $ Mono

instanciate :: Context s scope -> Position -> Forall s scope -> ST s (Type s scope, Instanciation s scope)
instanciate context position scheme = do
  (typex, instanciation) <- instanciateOver context position $ scheme
  pure (typex, Instanciationx instanciation)

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a

-- todo, figure out how to make sure no unification occurs during solving
liftST :: ST s a -> Solve s a
liftST = Solve

solveEvidence :: Position -> Evidence s scope -> Solve s (Simple.Evidence scope)
solveEvidence position (Evidencex evidence) = Evidence.solve position evidence

solveInstanciation :: Position -> Instanciation s scope -> Solve s (Simple.Instanciation scope)
solveInstanciation position (Instanciationx instanciation) = Instanciation.solve position instanciation

solveConstraint :: Position -> Constraint s scope -> Solve s (Simple.Constraint scope)
solveConstraint position (Constraintx constraint) = Constraint.solve position constraint

solveForall :: Position -> Forall s scope -> Solve s (Simple.Forall scope)
solveForall = solveForallOver (SolveForall solve)

solveForallOver ::
  SolveForall source target ->
  Position ->
  ForallOver source s scope ->
  Solve s (Simple.ForallOver target Vacuous scope)
solveForallOver solve position scheme = Forall.solve solve position scheme

fresh :: Type s scope -> ST s (Type s scope)
fresh (Typex typex) = Typex <$> Type.fresh typex

mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
mark context position erasure (Typex typex) = Type.mark context position erasure typex

solve :: Position -> Type s scope -> Solve s (Simple.Type scope)
solve position (Typex typex) = Type.solve position typex

-- | Unify two types
--
-- The first argument is the expected type.
-- The second argument is the actual type.
--
-- Both arguments must be well kinded, though they may have different kinds.
-- Alternatively, they may be untypeable, in which case they never unify with
-- unification variables and instead only do syntatic equality.
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
unify context position (Typex type1) (Typex type2) = Type.unify context position type1 type2

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrain context position classx (Typex argument) = do
  evidence <- Type.constrain context position classx argument
  pure $ Evidencex evidence

liftWith :: Strict.Vector (Type s scope) -> TypeF Vacuous (Scope.Local ':+ scope) -> Type s scope
liftWith substitution typex = Typex $ Type.liftWith (Strict.Vector.toLazy $ runTypex <$> substitution) typex

liftWith' ::
  Strict.Vector (Type s scope) ->
  TypeF Vacuous (Scope.Local ':+ Scope.Local ':+ scope) ->
  Type s (Scope.Local ':+ scope)
liftWith' substitution typex = Typex $ Substitute.mapType substitute typex
  where
    substitute = Substitute.Over $ Substitute Shift.Id wrapped Vector.empty
    wrapped = Strict.Vector.toLazy $ fmap runTypex substitution

lift :: TypeF Vacuous scope -> Type s scope
lift = liftWith Strict.Vector.empty . shift
