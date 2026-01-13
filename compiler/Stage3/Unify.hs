module Stage3.Unify
  ( Type,
    Scheme (..),
    SchemeOver,
    Constraint,
    Evidence,
    Instanciation,
    Algebra,
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
    proof,
    super,
    fresh,
    unify,
    constrain,
    solve,
    solveEvidence,
    solveInstanciation,
    instanciate,
    instanciateOver,
  )
where

import Control.Monad (liftM2, zipWithM_)
import Control.Monad.ST (ST)
import Data.Foldable (for_, toList, traverse_)
import qualified Data.Kind
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( ambiguousType,
    constraintError,
    escapingType,
    occurenceError,
    unificationError,
    unsupportedFeatureConstraintedTypeDefaulting,
  )
import qualified Stage1.Lexer as Lexer
import Stage1.Position (Position)
import qualified Stage1.Printer as Stage1 (build)
import qualified Stage1.Tree.Type as Stage1 (Type (Call, argument, function, startPosition), print)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local (Index (Local, Shift))
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Term as Term.Table
import qualified Stage2.Index.Table.Type as Type ((!))
import qualified Stage2.Index.Table.Type as Type.Table
import qualified Stage2.Index.Type as Type (Index, unlocal)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Local as Label (LocalBinding (..))
import qualified Stage2.Label.Context as Label (Context (..))
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Type as Stage2
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.Context as Context
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Check.LocalBinding as Local (Constraint (..), LocalBinding (..))
import Stage3.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Index.Evidence as Evidence
import {-# SOURCE #-} qualified Stage3.Simple.Builtin as Builtin
import qualified Stage3.Simple.Constraint as Simple (argument)
import qualified Stage3.Simple.Constraint as Simple.Constraint
import qualified Stage3.Simple.Data as Simple.Data
import qualified Stage3.Simple.Evidence as Simple (Evidence (..))
import qualified Stage3.Simple.Evidence as Simple.Evidence
import qualified Stage3.Simple.Instanciation as Simple (Instanciation (..))
import qualified Stage3.Simple.Type as Simple (Type (..), instanciate, lift)
import {-# SOURCE #-} Stage3.Simple.TypeDeclaration (assumeData)
import Prelude hiding (head)

type Type :: Data.Kind.Type -> Environment -> Data.Kind.Type
data Type s scopes where
  Logical :: !(STRef s (Box s scopes)) -> Type s scopes
  -- |
  -- Bring a type from a higher scope into current scope
  Shift :: !(Type s scopes) -> Type s (scope ':+ scopes)
  Variable :: !(Local.Index scopes) -> Type s scopes
  Constructor :: !(Type2.Index scopes) -> Type s scopes
  Call :: !(Type s scopes) -> !(Type s scopes) -> Type s scopes
  Function :: !(Type s scopes) -> !(Type s scopes) -> Type s scopes
  Type :: !(Type s scopes) -> Type s scopes
  Constraint :: Type s scopes
  Small :: Type s scopes
  Large :: Type s scopes
  Universe :: Type s scopes

instance Shift (Type s) where
  shift = Shift

data SchemeOver typex s scope = SchemeOver
  { parameters :: !(Strict.Vector (Type s scope)),
    constraints :: !(Strict.Vector (Constraint s scope)),
    result :: !(typex s (Scope.Local ':+ scope))
  }

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

data Constraint s scope = Constraint'
  { classx :: !(Type2.Index scope),
    head :: Int,
    arguments :: !(Strict.Vector (Type s (Scope.Local ':+ scope)))
  }

data Evidence s scope where
  Proof :: !(Evidence.Index scope) -> !(Strict.Vector (Evidence s scope)) -> Evidence s scope
  Super :: !(Evidence s scope) -> !Int -> Evidence s scope
  Logical' :: !(STRef s (Box' s scope)) -> Evidence s scope
  Shift' :: Evidence s scopes -> Evidence s (scope ':+ scopes)

data Box s scope
  = Unsolved
      { kindx :: !(Type s scope),
        constraintsx :: !(Map (Type2.Index scope) (Delay s scope))
      }
  | Solved !(Type s scope)

newtype Instanciation s scope = Instanciation
  { runInstanciation :: Strict.Vector (Evidence s scope)
  }

data Delay s scope = Delay
  { arguments' :: [Type s scope],
    evidence :: Evidence s scope
  }

data Box' s scope
  = Solved' (Evidence s scope)
  | Unsolved' {}

class Algebra typex where
  substitute :: Strict.Vector (Type s scope) -> typex s (Scope.Local ':+ scope) -> typex s scope

instance Algebra Type where
  substitute replacements = \case
    Logical _ -> error "logic variables can not be under a scheme"
    Shift typex -> typex
    Variable (Local.Local index) -> replacements Strict.Vector.! index
    Variable (Local.Shift index) -> Variable index
    Constructor index -> Constructor (Type2.map Type.unlocal index)
    Call function argument ->
      Call (substitute replacements function) (substitute replacements argument)
    Function parameter result ->
      Function (substitute replacements parameter) (substitute replacements result)
    Type universe -> Type (substitute replacements universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe

scheme ::
  Strict.Vector.Vector (Type s scope) ->
  Strict.Vector.Vector (Constraint s scope) ->
  Type s (Scope.Local ':+ scope) ->
  Scheme s scope
scheme parameters constraints result =
  Scheme
    (schemeOver parameters constraints result)

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
  Constraint'
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

proof :: Evidence.Index scope -> Strict.Vector (Evidence s scope) -> Evidence s scope
proof = Proof

super :: Evidence s scope -> Int -> Evidence s scope
super = Super

fresh :: Type s scope -> ST s (Type s scope)
fresh kindx = do
  box <- newSTRef $! Unsolved {kindx, constraintsx = Map.empty}
  pure (Logical box)

-- | Unify two types
--
-- The first argument is the expected type.
-- The second argument is the actual type.
--
-- Both arguments must be well kinded, though they may have different kinds.
-- Alternatively, they may be untypeable, in which case they never unify with
-- unification variables and instead only do syntatic equality.
unify :: forall s scope. Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
unify context_ position term1_ term2_ = unifyWith context_ term1_ term2_
  where
    unifyWith :: forall scope. Context s scope -> Type s scope -> Type s scope -> ST s ()
    unifyWith context = unify
      where
        unify (Logical reference) (Logical reference')
          | reference == reference' = pure ()
          | otherwise = do
              box <- readSTRef reference
              box' <- readSTRef reference'
              combine box box'
          where
            combine (Solved term) (Solved term') = unify term term'
            combine (Solved (Logical reference)) Unsolved {kindx, constraintsx} = do
              box <- readSTRef reference
              combine box Unsolved {kindx, constraintsx}
            combine (Solved term) Unsolved {kindx, constraintsx} = do
              occurs context position reference' term
              typeCheck context position kindx term
              reconstrain context position constraintsx term
              writeSTRef reference' $! Solved term
            combine Unsolved {kindx, constraintsx} (Solved (Logical reference')) = do
              box' <- readSTRef reference'
              combine Unsolved {kindx, constraintsx} box'
            combine Unsolved {kindx, constraintsx} (Solved term') = do
              occurs context position reference term'
              typeCheck context position kindx term'
              reconstrain context position constraintsx term'
              writeSTRef reference $! Solved term'
            combine
              Unsolved {kindx = kind1, constraintsx = constraints1}
              Unsolved {kindx = kind2, constraintsx = constraints2}
                | constraints1 <- fmap pure constraints1,
                  constraints2 <- fmap pure constraints2,
                  let merge left right = do
                        left@Delay {arguments' = arguments1, evidence = evidence1} <- left
                        Delay {arguments' = arguments2, evidence = evidence2} <- right
                        if length arguments1 == length arguments2
                          then traverse_ (uncurry unify) (zip arguments1 arguments2)
                          else error "different parameter length"
                        unifyEvidence evidence1 evidence2
                        pure left =
                    do
                      Stage3.Unify.unify context position kind1 kind2
                      constraintsx <- sequence $ Map.unionWith merge constraints1 constraints2
                      writeSTRef reference' $! Unsolved {kindx = kind1, constraintsx}
                      writeSTRef reference $! Solved (Logical reference')
        unify (Logical reference) term' =
          readSTRef reference >>= \case
            Unsolved {kindx, constraintsx} -> do
              occurs context position reference term'
              typeCheck context position kindx term'
              reconstrain context position constraintsx term'
              writeSTRef reference $! (Solved term')
            Solved term -> unify term term'
        unify term (Logical reference') =
          readSTRef reference' >>= \case
            Unsolved {kindx, constraintsx} -> do
              occurs context position reference' term
              typeCheck context position kindx term
              reconstrain context position constraintsx term
              writeSTRef reference' $! (Solved term)
            Solved term' -> unify term term'
        -- Types involving a higher scope must be solved in said scope
        unify (Shift term) term' = do
          term' <- unshift context position term'
          unifyWith (Shift.unshift context) term term'
        unify term (Shift term') = do
          term <- unshift context position term
          unifyWith (Shift.unshift context) term term'
        unify (Variable index) (Variable index')
          | index == index' = pure ()
          | otherwise = mismatch
        unify (Constructor index) (Constructor index')
          | index == index' = pure ()
          | otherwise = mismatch
        unify (Call term1 term2) (Call term1' term2') = do
          unify term1 term1'
          unify term2 term2'
        unify (Function type1 type2) (Function type1' type2') = do
          unify type1 type1'
          unify type2 type2'
        unify (Function type1 type2) (Call function' type2') = do
          typeCheck context position typex type1
          typeCheck context position typex type2
          unify (Call arrow type1) function'
          unify type2 type2'
        unify (Call function type2) (Function type1' type2') = do
          typeCheck context position typex type1'
          typeCheck context position typex type2'
          unify function (Call arrow type1')
          unify type2 type2'
        unify (Type universe) (Type universe') = do
          unify universe universe'
        unify Constraint Constraint = pure ()
        unify Small Small = pure ()
        unify Large Large = pure ()
        unify Universe Universe = pure ()
        unify _ _ = mismatch

    mismatch :: ST s ()
    mismatch = abort position (Unify context_ term1_ term2_)

-- unification between evidence should never fail
unifyEvidence :: Evidence s scope -> Evidence s scope -> ST s ()
unifyEvidence (Logical' reference) (Logical' reference')
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved' evidence) (Solved' evidence') = unifyEvidence evidence evidence'
    combine Unsolved' evidence' = writeSTRef reference evidence'
    combine evidence Unsolved' = writeSTRef reference' evidence
unifyEvidence (Logical' reference) evidence' =
  readSTRef reference >>= \case
    Solved' evidence -> unifyEvidence evidence evidence'
    Unsolved' -> writeSTRef reference (Solved' evidence')
unifyEvidence evidence (Logical' reference') =
  readSTRef reference' >>= \case
    Solved' evidence' -> unifyEvidence evidence evidence'
    Unsolved' -> writeSTRef reference' (Solved' evidence)
unifyEvidence (Proof index arguments) (Proof index' arguments')
  | index == index' =
      sequence_ $ Strict.Vector.zipWith unifyEvidence arguments arguments'
unifyEvidence (Shift' evidence) evidence' = do
  evidence' <- unshiftEvidence evidence'
  unifyEvidence evidence evidence'
unifyEvidence evidence (Shift' evidence') = do
  evidence <- unshiftEvidence evidence
  unifyEvidence evidence evidence'
unifyEvidence _ _ = error "unify evidence can't fail"

-- todo merge this with Stage3.Temporary.Type checking somehow
typeCheck :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
typeCheck context_ position = typeCheckWith context_
  where
    typeCheckWith :: Context s scope -> Type s scope -> Type s scope -> ST s ()
    typeCheckWith context@Context {localEnvironment, typeEnvironment} = typeCheck
      where
        typeCheck kindx = \case
          Logical reference ->
            readSTRef reference >>= \case
              Solved typex -> typeCheck kindx typex
              Unsolved {kindx = kind'} -> do
                unify context position kindx kind'
          Shift typex -> do
            kindx <- unshift context position kindx
            typeCheckWith (Shift.unshift context) kindx typex
          Variable index -> case localEnvironment Local.Table.! index of
            Local.Rigid {rigid} -> do
              unify context position kindx (Simple.lift rigid)
            Local.Wobbly {wobbly} -> do
              unify context position kindx wobbly
          Constructor constructor -> do
            kind <- Builtin.kind (pure . Simple.lift) indexType indexLift constructor
            unify context position kind kindx
            where
              indexType index =
                case typeEnvironment Type.Table.! index of
                  TypeBinding {kind = kind'} -> do
                    Simple.lift <$> kind'
              indexLift constructor = do
                let Constructor.Index {typeIndex, constructorIndex} = constructor
                datax <- do
                  let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
                  Builtin.index pure get typeIndex
                DataInstance {types, constructors} <-
                  Simple.Data.instanciate datax
                let root = Stage3.Unify.constructor typeIndex
                    base = foldl call root types
                    ConstructorInstance {entries} =
                      constructors Strict.Vector.! constructorIndex
                pure $ foldr function base entries
          Constraint -> unify context position kind kindx
          Small -> unify context position universe kindx
          Large -> unify context position universe kindx
          Universe -> error "type checking universe"
          Call functionx parameter -> do
            level <- fresh universe
            parameterKind <- fresh (typeWith level)
            typeCheck (function parameterKind kindx) functionx
            typeCheck parameterKind parameter
          Function argument result -> do
            level <- fresh universe
            level' <- fresh universe
            unify context position (typeWith level') kindx
            typeCheck (typeWith level) argument
            typeCheck (typeWith level') result
          Type universe -> do
            unify context position small universe
            unify context position kind kindx

occurs :: Context s scope -> Position -> STRef s (Box s scope) -> Type s scope -> ST s ()
occurs context position reference term_ = occurs term_
  where
    occurs = \case
      Logical reference'
        | reference == reference' -> occurrenece
        | otherwise ->
            readSTRef reference' >>= \case
              Unsolved {} -> pure ()
              Solved typex -> occurs typex
      Shift _ -> pure ()
      Variable _ -> pure ()
      Constructor _ -> pure ()
      Call term1 term2 -> do
        occurs term1
        occurs term2
      Function argument result -> do
        occurs argument
        occurs result
      Type universe -> do
        occurs universe
      Constraint -> pure ()
      Small -> pure ()
      Large -> pure ()
      Universe -> pure ()
    occurrenece = abort position (Occurs context reference term_)

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrain context position classx term = constrainWith context position classx term []

constrainWith ::
  forall s scope.
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  [Type s scope] ->
  ST s (Evidence s scope)
constrainWith context_ position classx_ term_ arguments_ = constrainWith context_ classx_ term_ arguments_
  where
    constrainWith ::
      forall scope.
      Context s scope ->
      Type2.Index scope ->
      Type s scope ->
      [Type s scope] ->
      ST s (Evidence s scope)
    constrainWith _ Type2.Num (Constructor Type2.Integer) [] =
      pure $ Proof Evidence.NumInteger Strict.Vector.empty
    constrainWith _ Type2.Num (Constructor Type2.Int) [] =
      pure $ Proof Evidence.NumInt Strict.Vector.empty
    constrainWith _ Type2.Enum (Constructor Type2.Integer) [] =
      pure $ Proof Evidence.EnumInteger Strict.Vector.empty
    constrainWith _ Type2.Enum (Constructor Type2.Int) [] =
      pure $ Proof Evidence.EnumInt Strict.Vector.empty
    constrainWith context@Context {typeEnvironment} classx term@(Logical reference) arguments =
      readSTRef reference >>= \case
        Solved term -> constrainWith context classx term arguments
        Unsolved {kindx, constraintsx} -> do
          case Map.lookup classx constraintsx of
            Nothing -> do
              target <- fresh kind
              let indexType index =
                    case typeEnvironment Type.Table.! index of
                      TypeBinding {kind = kind'} -> do
                        Simple.lift <$> kind'
                  indexLift constructor = do
                    let Constructor.Index {typeIndex, constructorIndex} = constructor
                    datax <- do
                      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
                      Builtin.index pure get typeIndex
                    DataInstance {types, constructors} <-
                      Simple.Data.instanciate datax
                    let root = Stage3.Unify.constructor typeIndex
                        base = foldl call root types
                        ConstructorInstance {entries} =
                          constructors Strict.Vector.! constructorIndex
                    pure $ foldr function base entries
              real <- Builtin.kind (pure . Simple.lift) indexType indexLift classx
              unify context position (function target constraint) real

              typeCheck context position target (foldl Call term arguments)
              logical <- newSTRef Unsolved' {}
              let delay = Delay {arguments' = arguments, evidence = Logical' logical}
              writeSTRef reference $! Unsolved {kindx, constraintsx = Map.insert classx delay constraintsx}
              pure $ Logical' logical
            Just Delay {arguments', evidence}
              | length arguments == length arguments' -> do
                  zipWithM_ (unify context position) arguments arguments'
                  pure evidence
              | otherwise -> error "error argument length doesn't match"
    constrainWith context classx (Shift term) arguments = do
      arguments <- traverse (unshift context position) arguments
      let quit = abort position $ Unshift context (Constructor classx)
      classx <- Shift.partialUnshift quit classx
      Shift' <$> constrainWith (Shift.unshift context) classx term arguments
    constrainWith context@Context {localEnvironment} classx (Variable index) arguments
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just Local.Constraint {arguments = arguments', evidence} <- Map.lookup classx constraints,
        arguments' <- [Simple.lift argument | argument <- toList arguments'],
        length arguments' == length arguments = do
          traverse (uncurry $ unify context position) (zip arguments arguments')
          pure (Simple.Evidence.lift evidence)
    constrainWith context@Context {typeEnvironment} (Type2.Index classx) (Constructor index) arguments
      | TypeBinding {classInstances} <- typeEnvironment Type.Table.! classx,
        Just instancex <- Map.lookup index classInstances = do
          TypeBinding.Instance dependencies <- instancex
          arguments <- for dependencies $ \case
            constraint@Simple.Constraint.Constraint {classx}
              | argument <-
                  Simple.instanciate
                    (Strict.Vector.fromList arguments)
                    (Simple.argument constraint) -> do
                  constrain context position classx argument
          pure (Proof (Evidence.Class classx index) arguments)
    constrainWith context@Context {typeEnvironment} classx (Constructor (Type2.Index index)) arguments
      | TypeBinding {dataInstances} <- typeEnvironment Type.Table.! index,
        Just instancex <- Map.lookup classx dataInstances = do
          TypeBinding.Instance dependencies <- instancex
          arguments <- for dependencies $ \case
            constraint@Simple.Constraint.Constraint {classx}
              | argument <-
                  Simple.instanciate
                    (Strict.Vector.fromList arguments)
                    (Simple.argument constraint) -> do
                  constrain context position classx argument
          pure (Proof (Evidence.Data classx index) arguments)
    constrainWith context classx (Call function argument) arguments =
      constrainWith context classx function (argument : arguments)
    constrainWith context classx (Function argument result) arguments = do
      typeCheck context position typex argument
      typeCheck context position typex result
      constrainWith context classx (arrow `Call` argument `Call` result) arguments
    constrainWith _ _ _ _ =
      abort position (Constrain context_ classx_ term_ arguments_)

reconstrain :: Context s scope -> Position -> Map (Type2.Index scope) (Delay s scope) -> Type s scope -> ST s ()
reconstrain context position constraints term =
  for_ (Map.toList constraints) $ \(classx, Delay {arguments', evidence}) -> do
    evidence' <- constrainWith context position classx term arguments'
    unifyEvidence evidence evidence'

unshift ::
  forall s scope scopes.
  Context s (scope ':+ scopes) ->
  Position ->
  Type s (scope ':+ scopes) ->
  ST s (Type s scopes)
unshift context position typex = unshift typex
  where
    unshift = \case
      Logical reference ->
        readSTRef reference >>= \case
          Unsolved {kindx, constraintsx} -> do
            let unshiftDelay (key, Delay {arguments', evidence}) = do
                  key <- Shift.partialUnshift misshift key
                  arguments' <- traverse unshift arguments'
                  evidence <- unshiftEvidence evidence
                  pure (key, Delay {arguments', evidence})
            kindx <- unshift kindx
            -- todo
            -- there's no Map.traverseKeysMonotonic
            constraintsx <- Map.fromAscList <$> traverse unshiftDelay (Map.toAscList constraintsx)
            box <- newSTRef $! Unsolved {kindx, constraintsx}
            let term = Logical box
            writeSTRef reference $! Solved $ Shift term
            pure term
          Solved term -> unshift term
      Shift term -> pure term
      Variable index -> Variable <$> Shift.partialUnshift misshift index
      Constructor index -> Constructor <$> Shift.partialUnshift misshift index
      Call term1 term2 -> do
        term1 <- unshift term1
        term2 <- unshift term2
        pure (Call term1 term2)
      Function argument result -> do
        argument <- unshift argument
        result <- unshift result
        pure (Function argument result)
      Type universe -> do
        universe <- unshift universe
        pure (Type universe)
      Constraint -> pure Constraint
      Small -> pure Small
      Large -> pure Large
      Universe -> pure Universe
    misshift = do
      abort position $ Unshift context typex

unshiftEvidence :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
unshiftEvidence = \case
  Proof index arguments -> do
    arguments <- traverse unshiftEvidence arguments
    let fail = error "unshift can't fail"
    pure $ Proof (Shift.map (Shift.Unshift fail) index) arguments
  Logical' reference -> do
    readSTRef reference >>= \case
      Solved' evidence -> unshiftEvidence evidence
      Unsolved' -> do
        box <- newSTRef $! Unsolved'
        writeSTRef reference (Solved' $ Shift' $ Logical' box)
        pure $ Logical' box
  Shift' evidence -> pure evidence
  Super evidence index -> do
    evidence <- unshiftEvidence evidence
    pure $ Super evidence index

defaultFrom,
  defaultUniverse ::
    Position ->
    Map (Type2.Index scope) (Delay s scope) ->
    Type s scope ->
    ST s (Simple.Type scope)
defaultFrom position constraints kind = case kind of
  Logical reference ->
    readSTRef reference >>= \case
      Solved kind -> defaultFrom position constraints kind
      Unsolved {} -> ambiguousType position
  Type universe -> defaultUniverse position constraints universe
  _ -> ambiguousType position
defaultUniverse position constraints universe = case universe of
  Logical reference ->
    readSTRef reference >>= \case
      Solved kind -> defaultUniverse position constraints kind
      Unsolved {} -> pure $ Simple.Type Simple.Small
  Small
    | null constraints -> pure $ Simple.Constructor (Type2.Tuple 0)
    | otherwise -> unsupportedFeatureConstraintedTypeDefaulting position
  Large
    | null constraints -> pure $ Simple.Type Simple.Small
    | otherwise -> error "unexpected kind constraints"
  _ -> error "unexpected universe kind"

instanciate :: Context s scope -> Position -> Scheme s scope -> ST s (Type s scope, Instanciation s scope)
instanciate context position Scheme {runScheme} =
  instanciateOver context position runScheme

instanciateOver ::
  (Algebra typex) =>
  Context s scope ->
  Position ->
  SchemeOver typex s scope ->
  ST s (typex s scope, Instanciation s scope)
instanciateOver context position SchemeOver {parameters, constraints, result} = do
  fresh <- traverse fresh parameters
  evidence <- for constraints $ \Constraint' {classx, head, arguments} -> do
    head <- pure $ fresh Strict.Vector.! head
    arguments <- pure $ toList $ fmap (substitute fresh) arguments
    constrainWith context position classx head arguments
  pure $ (substitute fresh result, Instanciation evidence)

solve :: Position -> Type s scope -> ST s (Simple.Type scope)
solve position = solve
  where
    solve :: Type s scope -> ST s (Simple.Type scope)
    solve = \case
      Logical reference ->
        readSTRef reference >>= \case
          Solved typex -> solve typex
          Unsolved {kindx, constraintsx} -> defaultFrom position constraintsx kindx
      Shift typex -> shift <$> solve typex
      Variable name -> pure $ Simple.Variable name
      Constructor index -> pure $ Simple.Constructor index
      Call function argument -> do
        function <- solve function
        argument <- solve argument
        pure $ Simple.Call function argument
      Function argument result -> do
        argument <- solve argument
        result <- solve result
        pure $ Simple.Function argument result
      Type universe -> do
        universe <- solve universe
        pure $ Simple.Type universe
      Constraint -> pure $ Simple.Constraint
      Small -> pure $ Simple.Small
      Large -> pure $ Simple.Large
      Universe -> pure $ Simple.Universe

solveEvidence :: Position -> Evidence s scope -> ST s (Simple.Evidence scope)
solveEvidence position = \case
  Proof proof arguments -> do
    arguments <- traverse (solveEvidence position) arguments
    pure $ Simple.Proof {proof, arguments}
  Super base index -> do
    base <- solveEvidence position base
    pure $ Simple.Super {base, index}
  Shift' evidence -> shift <$> solveEvidence position evidence
  Logical' reference ->
    readSTRef reference >>= \case
      Solved' evidence -> solveEvidence position evidence
      Unsolved' -> ambiguousType position

solveInstanciation :: Position -> Instanciation s scope -> ST s (Simple.Instanciation scope)
solveInstanciation position (Instanciation instanciation) = do
  instanciation <- traverse (solveEvidence position) instanciation
  pure $ Simple.Instanciation instanciation

data Error s where
  Unify :: Context s scope -> Type s scope -> Type s scope -> Error s
  Occurs :: Context s scope -> STRef s (Box s scope) -> Type s scope -> Error s
  Constrain :: Context s scope -> Type2.Index scope -> Type s scope -> [Type s scope] -> Error s
  Unshift :: Context s (scope ':+ scopes) -> Type s (scope ':+ scopes) -> Error s

abort :: Position -> Error s -> ST s a
abort position = \case
  Unify context term1 term2 -> do
    unsolved <- nub <$> liftM2 (++) (collect term1) (collect term2)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term1 <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term1
    term2 <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term2
    unificationError position term1 term2
  Occurs context reference term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
        infinite = case lookup (Collect reference) labeled of
          Nothing -> error "bad occurs lookup"
          Just variable ->
            let ast = Stage2.Variable {startPosition = (), variable}
             in Stage1.build $ Stage1.print $ Stage2.label temporary ast
    term <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term
    occurenceError position infinite term
  Constrain context classx constructor arguments -> do
    unsolved <- nub . concat <$> traverse collect (constructor : arguments)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    fabricated <- fmap (Stage2.label temporary) <$> traverse (fabricate Shift.Shift labeled) arguments
    function <- Stage2.label temporary <$> fabricate Shift.Shift labeled (Constructor classx)
    argument <- Stage2.label temporary <$> fabricate Shift.Shift labeled constructor
    let head =
          Stage1.Call
            { startPosition = (),
              function,
              argument
            }
        call function argument =
          Stage1.Call
            { startPosition = (),
              function,
              argument
            }
        term = Stage1.build $ Stage1.print $ foldl call head fabricated
    constraintError position term
  Unshift context term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term
    escapingType position term
  where
    temporaries :: Int -> Context s scopes -> Label.Context (Scope.Local ':+ scopes)
    temporaries length context = case Context.label context of
      Label.Context {terms, locals, types} ->
        Label.Context
          { terms = Term.Table.Local terms,
            locals = Local.Table.Local names locals,
            types = Type.Table.Local types
          }
        where
          names = Vector.fromList $ do
            i <- [0 .. length]
            let name = Lexer.variableIdentifier (Text.pack $ "__flexible_" ++ show i)
            pure Label.LocalBinding {name}
    fabricate ::
      Shift.Category scope scope' ->
      [(Collected s scope, Local.Index scope')] ->
      Type s scope ->
      ST s (Stage2.Type () scope')
    fabricate category names = \case
      Logical reference ->
        readSTRef reference >>= \case
          Solved typex -> fabricate category names typex
          Unsolved {} -> case lookup (Collect reference) names of
            Just variable ->
              pure
                Stage2.Variable
                  { startPosition = (),
                    variable
                  }
            Nothing -> error "uncollected variable"
      Shift typex -> fabricate (category Shift.:. Shift.Shift) names' typex
        where
          names' = [(collect, name) | (Reach collect, name) <- names]
      Variable variable ->
        pure $
          Stage2.Variable
            { startPosition = (),
              variable = Shift.map category variable
            }
      Constructor constructor ->
        pure $
          Shift.map category $
            Stage2.Constructor
              { startPosition = (),
                constructorPosition = (),
                constructor
              }
      Call function argument -> do
        function <- fabricate category names function
        argument <- fabricate category names argument
        pure $ Stage2.Call {function, argument}
      Function parameter result -> do
        parameter <- fabricate category names parameter
        result <- fabricate category names result
        pure $
          Stage2.Function
            { parameter,
              operatorPosition = (),
              result
            }
      Type universe -> do
        universe <- fabricate category names universe
        pure $
          Stage2.Type
            { startPosition = (),
              universe
            }
      Constraint -> pure $ Stage2.Constraint {startPosition = ()}
      Small -> pure $ Stage2.Small {startPosition = ()}
      Large -> pure $ Stage2.Large {startPosition = ()}
      Universe -> pure $ Stage2.Universe {startPosition = ()}

-- todo, this is O(n^2) due to STRefs not having an order
-- especially not a heterogeneous order

data Collected s scopes where
  Collect :: STRef s (Box s scopes) -> Collected s scopes
  Reach :: Collected s scopes -> Collected s (scope ':+ scopes)

instance Eq (Collected s scopes) where
  Collect left == Collect right = left == right
  Reach left == Reach right = left == right
  _ == _ = False

collect :: Type s scopes -> ST s [Collected s scopes]
collect = \case
  Logical reference ->
    readSTRef reference >>= \case
      Solved typex -> collect typex
      Unsolved {} -> pure [Collect reference]
  Shift typex -> fmap Reach <$> collect typex
  Variable {} -> pure []
  Constructor {} -> pure []
  Call function argument -> do
    function <- collect function
    argument <- collect argument
    pure (function ++ argument)
  Function argument result -> do
    argument <- collect argument
    result <- collect result
    pure $ argument ++ result
  Type universe -> do
    collect universe
  Constraint -> pure []
  Small -> pure []
  Large -> pure []
  Universe -> pure []
