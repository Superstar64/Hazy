module Stage3.Unify.Type where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Foldable (for_, toList, traverse_)
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (ambiguousType, unsupportedFeatureConstraintedTypeDefaulting)
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local (Index (..))
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Type as Type ((!))
import qualified Stage2.Index.Table.Type as Type.Table
import qualified Stage2.Index.Type as Type (unlocal)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Check.LocalBinding as Local (Constraint (..), LocalBinding (..))
import Stage3.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Index.Evidence as Evidence (Builtin (..), Index (..))
import qualified Stage3.Simple.Data as Simple.Data
import qualified Stage3.Simple.Evidence as Simple.Evidence (lift)
import qualified Stage3.Simple.Type as Simple (instanciate, lift)
import Stage3.Unify.Class
  ( Collected (..),
    Collector (..),
    Functor (..),
    Generalizable (..),
    Instantiatable (..),
    Substitute (..),
    Zonk (..),
    Zonker (..),
  )
import qualified Stage3.Unify.Class as Class
import {-# SOURCE #-} Stage3.Unify.Error (Error (..), abort)
import Stage3.Unify.Evidence (Evidence)
import qualified Stage3.Unify.Evidence as Evidence (Box (..), Evidence (..), unify, unshift)
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import qualified Stage4.Tree.Constraint as Simple (argument)
import qualified Stage4.Tree.Constraint as Simple.Constraint
import qualified Stage4.Tree.Type as Simple (Type (..))
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration (assumeData)
import Prelude hiding (Functor, head, map)

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

data Box s scope
  = Unsolved
      { kind :: !(Type s scope),
        constraints :: !(Map (Type2.Index scope) (Delay s scope))
      }
  | Solved !(Type s scope)

data Delay s scope = Delay
  { arguments :: [Type s scope],
    evidence :: Evidence s scope
  }

instance Shift (Type s) where
  shift = Shift

instance Functor (Type s) where
  map Class.Shift typex = Shift typex
  map (Class.Over category) (Shift typex) = Shift (Class.map category typex)
  map (Class.Over _) (Logical _) = error "can't map over logical"
  map category typex = case typex of
    Variable index -> Variable (Shift.map (Class.general category) index)
    Constructor index -> Constructor (Shift.map (Class.general category) index)
    Call function argument -> Call (Class.map category function) (Class.map category argument)
    Function parameter result -> Function (Class.map category parameter) (Class.map category result)
    Type universe -> Type (Class.map category universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe

instance Zonk Type where
  zonk Zonker = zonk
    where
      zonk :: Type s scope -> ST s (Type s scope)
      zonk = \case
        Logical reference ->
          readSTRef reference >>= \case
            Solved solved -> zonk solved
            Unsolved {} -> pure $ Logical reference
        Shift typex -> Shift <$> zonk typex
        Variable index -> pure $ Variable index
        Constructor index -> pure $ Constructor index
        Call function argument -> do
          function <- zonk function
          argument <- zonk argument
          pure $ Call function argument
        Function parameter result -> do
          parameter <- zonk parameter
          result <- zonk result
          pure $ Function parameter result
        Type universe -> Type <$> zonk universe
        Constraint -> pure Constraint
        Small -> pure Small
        Large -> pure Large
        Universe -> pure Universe

instance Generalizable Type where
  collect Collector = collect
    where
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

instance Instantiatable Type where
  substitute replacements = \case
    Logical _ -> error "logic variables can not be under a scheme"
    Call function argument ->
      Call (substitute replacements function) (substitute replacements argument)
    Function parameter result ->
      Function (substitute replacements parameter) (substitute replacements result)
    Type universe -> Type (substitute replacements universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    typex -> case replacements of
      Substitute replacements -> case typex of
        Shift typex -> typex
        Variable (Local.Local index) -> replacements Strict.Vector.! index
        Variable (Local.Shift index) -> Variable index
        Constructor index -> Constructor (Type2.map Type.unlocal index)

fresh :: Type s scope -> ST s (Type s scope)
fresh kind = do
  box <- newSTRef $! Unsolved {kind, constraints = Map.empty}
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
            combine (Solved (Logical reference)) Unsolved {kind, constraints} = do
              box <- readSTRef reference
              combine box Unsolved {kind, constraints}
            combine (Solved term) Unsolved {kind, constraints} = do
              occurs context position reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $! Solved term
            combine Unsolved {kind, constraints} (Solved (Logical reference')) = do
              box' <- readSTRef reference'
              combine Unsolved {kind, constraints} box'
            combine Unsolved {kind, constraints} (Solved term') = do
              occurs context position reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $! Solved term'
            combine
              Unsolved {kind = kind1, constraints = constraints1}
              Unsolved {kind = kind2, constraints = constraints2}
                | constraints1 <- fmap pure constraints1,
                  constraints2 <- fmap pure constraints2,
                  let merge left right = do
                        left@Delay {arguments = arguments1, evidence = evidence1} <- left
                        Delay {arguments = arguments2, evidence = evidence2} <- right
                        if length arguments1 == length arguments2
                          then traverse_ (uncurry unify) (zip arguments1 arguments2)
                          else error "different parameter length"
                        Evidence.unify evidence1 evidence2
                        pure left =
                    do
                      Stage3.Unify.Type.unify context position kind1 kind2
                      constraints <- sequence $ Map.unionWith merge constraints1 constraints2
                      writeSTRef reference' $! Unsolved {kind = kind1, constraints}
                      writeSTRef reference $! Solved (Logical reference')
        unify (Logical reference) term' =
          readSTRef reference >>= \case
            Unsolved {kind, constraints} -> do
              occurs context position reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $ Solved term'
            Solved term -> unify term term'
        unify term (Logical reference') =
          readSTRef reference' >>= \case
            Unsolved {kind, constraints} -> do
              occurs context position reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $ Solved term
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
          typeCheck context position (Type Small) type1
          typeCheck context position (Type Small) type2
          unify (Call (Constructor Type2.Arrow) type1) function'
          unify type2 type2'
        unify (Call function type2) (Function type1' type2') = do
          typeCheck context position (Type Small) type1'
          typeCheck context position (Type Small) type2'
          unify function (Call (Constructor Type2.Arrow) type1')
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

-- todo merge this with Stage3.Temporary.Type checking somehow
typeCheck :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
typeCheck context_ position = typeCheckWith context_
  where
    typeCheckWith :: Context s scope -> Type s scope -> Type s scope -> ST s ()
    typeCheckWith context@Context {localEnvironment, typeEnvironment} = typeCheck
      where
        typeCheck kind = \case
          Logical reference ->
            readSTRef reference >>= \case
              Solved typex -> typeCheck kind typex
              Unsolved {kind = kind'} -> do
                unify context position kind kind'
          Shift typex -> do
            kind <- unshift context position kind
            typeCheckWith (Shift.unshift context) kind typex
          Variable index -> case localEnvironment Local.Table.! index of
            Local.Rigid {rigid} -> do
              unify context position kind (Simple.lift rigid)
            Local.Wobbly {wobbly} -> do
              unify context position kind wobbly
          Constructor constructor -> do
            kind' <- Builtin.kind (pure . Simple.lift) indexType indexLift constructor
            unify context position kind' kind
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
                let root = Constructor typeIndex
                    base = foldl Call root types
                    ConstructorInstance {entries} =
                      constructors Strict.Vector.! constructorIndex
                pure $ foldr Function base entries
          Constraint -> unify context position (Type Large) kind
          Small -> unify context position Universe kind
          Large -> unify context position Universe kind
          Universe -> error "type checking universe"
          Call functionx parameter -> do
            level <- fresh Universe
            parameterKind <- fresh (Type level)
            typeCheck (Function parameterKind kind) functionx
            typeCheck parameterKind parameter
          Function argument result -> do
            level <- fresh Universe
            level' <- fresh Universe
            unify context position (Type level') kind
            typeCheck (Type level) argument
            typeCheck (Type level') result
          Type universe -> do
            unify context position Small universe
            unify context position (Type Large) kind

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
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.NumInteger
    constrainWith _ Type2.Num (Constructor Type2.Int) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.NumInt
    constrainWith _ Type2.Enum (Constructor Type2.Bool) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EnumBool
    constrainWith _ Type2.Enum (Constructor Type2.Char) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EnumChar
    constrainWith _ Type2.Enum (Constructor Type2.Integer) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EnumInteger
    constrainWith _ Type2.Enum (Constructor Type2.Int) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EnumInt
    constrainWith _ Type2.Eq (Constructor Type2.Bool) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EqBool
    constrainWith _ Type2.Eq (Constructor Type2.Char) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EqChar
    constrainWith _ Type2.Eq (Constructor Type2.Integer) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EqInteger
    constrainWith _ Type2.Eq (Constructor Type2.Int) [] =
      pure $ Evidence.Variable $ Evidence.Builtin Evidence.EqInt
    constrainWith context@Context {typeEnvironment} classx term@(Logical reference) arguments =
      readSTRef reference >>= \case
        Solved term -> constrainWith context classx term arguments
        Unsolved {kind, constraints} -> do
          case Map.lookup classx constraints of
            Nothing -> do
              target <- fresh (Type Large)
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
                    let root = Constructor typeIndex
                        base = foldl Call root types
                        ConstructorInstance {entries} =
                          constructors Strict.Vector.! constructorIndex
                    pure $ foldr Function base entries
              real <- Builtin.kind (pure . Simple.lift) indexType indexLift classx
              unify context position (Function target Constraint) real

              typeCheck context position target (foldl Call term arguments)
              logical <- newSTRef Evidence.Unsolved {}
              let delay = Delay {arguments, evidence = Evidence.Logical logical}
              writeSTRef reference $! Unsolved {kind, constraints = Map.insert classx delay constraints}
              pure $ Evidence.Logical logical
            Just Delay {arguments = arguments', evidence}
              | length arguments == length arguments' -> do
                  zipWithM_ (unify context position) arguments arguments'
                  pure evidence
              | otherwise -> error "error argument length doesn't match"
    constrainWith context classx (Shift term) arguments = do
      arguments <- traverse (unshift context position) arguments
      let quit = abort position $ Unshift context (Constructor classx)
      classx <- Shift.partialUnshift quit classx
      Evidence.Shift <$> constrainWith (Shift.unshift context) classx term arguments
    constrainWith context@Context {localEnvironment} classx (Variable index) arguments
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just Local.Constraint {arguments = arguments', evidence} <- Map.lookup classx constraints,
        arguments' <- [Simple.lift argument | argument <- toList arguments'],
        length arguments' == length arguments = do
          traverse_ (uncurry $ unify context position) (zip arguments arguments')
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
          pure (proof (Evidence.Class classx index) arguments)
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
          pure (proof (Evidence.Data classx index) arguments)
    constrainWith context classx (Call function argument) arguments =
      constrainWith context classx function (argument : arguments)
    constrainWith context classx (Function argument result) arguments = do
      typeCheck context position (Type Small) argument
      typeCheck context position (Type Small) result
      constrainWith context classx (Constructor Type2.Arrow `Call` argument `Call` result) arguments
    constrainWith _ _ _ _ =
      abort position (Constrain context_ classx_ term_ arguments_)

    proof :: forall scope s. Evidence.Index scope -> Strict.Vector (Evidence s scope) -> Evidence s scope
    proof variable arguments
      | null arguments = Evidence.Variable variable
      | otherwise = Evidence.Call (Evidence.Variable variable) arguments

reconstrain :: Context s scope -> Position -> Map (Type2.Index scope) (Delay s scope) -> Type s scope -> ST s ()
reconstrain context position constraints term =
  for_ (Map.toList constraints) $ \(classx, Delay {arguments, evidence}) -> do
    evidence' <- constrainWith context position classx term arguments
    Evidence.unify evidence evidence'

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
          Unsolved {kind, constraints} -> do
            let unshiftDelay (key, Delay {arguments, evidence}) = do
                  key <- Shift.partialUnshift misshift key
                  arguments <- traverse unshift arguments
                  evidence <- Evidence.unshift evidence
                  pure (key, Delay {arguments, evidence})
            kind <- unshift kind
            -- todo
            -- there's no Map.traverseKeysMonotonic
            constraints <- Map.fromAscList <$> traverse unshiftDelay (Map.toAscList constraints)
            box <- newSTRef $! Unsolved {kind, constraints}
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

solve :: Position -> Type s scope -> ST s (Simple.Type scope)
solve position = solve
  where
    solve :: Type s scope -> ST s (Simple.Type scope)
    solve = \case
      Logical reference ->
        readSTRef reference >>= \case
          Solved typex -> solve typex
          Unsolved {kind, constraints} -> defaultFrom position constraints kind
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
      Constraint -> pure Simple.Constraint
      Small -> pure Simple.Small
      Large -> pure Simple.Large
      Universe -> pure Simple.Universe
