module Semantic.Unify.Type where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Builtin as Builtin (index, kind)
import qualified Core.Tree.Constraint as Simple (argument)
import qualified Core.Tree.Constraint as Simple.Constraint
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Evidence (EvidenceF)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Instanciation (InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF (..))
import qualified Core.Tree.Type as Simple (Type)
import {-# SOURCE #-} Core.Tree.TypeDeclaration (assumeData)
import Data.Foldable (for_, toList, traverse_)
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureConstraintedTypeDefaulting)
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.LocalBinding as Local (Constraint (..), LocalBinding (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Data as Simple.Data
import qualified Semantic.Check.Simple.Evidence as Simple.Evidence (lift)
import qualified Semantic.Check.Simple.Type as Simple (instanciate, lift)
import Semantic.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Local as Local (Index (..))
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Table.Type as Type ((!))
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type as Type (unlocal)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import Semantic.Shift (Shift (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Unify.Builtin as Builtin (constrain)
import Semantic.Unify.Class
  ( Collected (..),
    Collector (..),
    Generalizable (..),
    Instantiatable (..),
    Solve (..),
    Substitute (..),
    Zonk (..),
    Zonker (..),
  )
import {-# SOURCE #-} Semantic.Unify.Error (Error (..), abort)
import Semantic.Unify.Evidence (runEvidencex)
import qualified Semantic.Unify.Evidence as Evidence (Box (..), Logical (..), unify, unshift)
import Syntax.Position (Position)
import Prelude hiding (Functor, head, map)

type Type :: Data.Kind.Type -> Environment -> Data.Kind.Type
newtype Type s scopes = Typex {runTypex :: TypeF (Logical s) scopes}

data Logical s scopes where
  Box :: !(STRef s (Box s scopes)) -> Logical s scopes
  -- |
  -- Bring a type from a higher scope into current scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

data Box s scope
  = Unsolved
      { kind :: !(TypeF (Logical s) scope),
        constraints :: !(Map (Type2.Index scope) (Delay s scope)),
        erasure :: !Mask.Erasure
      }
  | Solved !(TypeF (Logical s) scope)

data Delay s scope = Delay
  { arguments :: [TypeF (Logical s) scope],
    evidence :: EvidenceF (Evidence.Logical s) scope
  }

instance Shift (Type s) where
  shift (Typex typex) = Typex (shift typex)

instance Shift (Logical s) where
  shift = Shift

instance Shift.Functor (Logical s) where
  map Shift.Shift logical = Shift logical
  map (Shift.Over category) (Shift logical) = Shift (Shift.map category logical)
  map Shift.Over {} Box {} = error "can't map over logical"
  map _ _ = error "unsupported shift"

instance Zonk Type where
  zonk Zonker (Typex typex) = Typex <$> zonk typex
    where
      zonk :: TypeF (Logical s) scope -> ST s (TypeF (Logical s) scope)
      zonk = \case
        Logical (Box reference) ->
          readSTRef reference >>= \case
            Solved solved -> zonk solved
            Unsolved {} -> pure $ Logical $ Box reference
        Logical (Shift logical) -> shift <$> zonk (Logical logical)
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
        Levity -> pure Levity

instance Generalizable Type where
  collect (Collector mask) (Typex typex) = collect typex
    where
      collect :: TypeF (Logical s) scope -> ST s [Collected s scope]
      collect = \case
        Logical (Box reference) ->
          readSTRef reference >>= \case
            Solved typex -> collect typex
            Unsolved {erasure}
              | Mask.valid mask erasure -> pure [Collect reference]
              | otherwise -> pure []
        Logical (Shift logical) -> fmap Reach <$> collect (Logical logical)
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
        Levity -> pure []

instance Instantiatable Type where
  substitute replacements (Typex typex) = Typex $ substitute replacements typex
    where
      substitute :: Substitute s scope scope' -> TypeF (Logical s) scope -> TypeF (Logical s) scope'
      substitute replacements = \case
        Logical Box {} -> error "logic variables can not be under a scheme"
        Call function argument ->
          Call (substitute replacements function) (substitute replacements argument)
        Function parameter result ->
          Function (substitute replacements parameter) (substitute replacements result)
        Type universe -> Type (substitute replacements universe)
        Constraint -> Constraint
        Small -> Small
        Large -> Large
        Universe -> Universe
        Levity -> Levity
        typex -> case replacements of
          Substitute replacements -> case typex of
            Logical (Shift typex) -> Logical typex
            Variable (Local.Local index)
              | Typex typex <- replacements Strict.Vector.! index -> typex
            Variable (Local.Shift index) -> Variable index
            Constructor index -> Constructor (Type2.map Type.unlocal index)

fresh :: TypeF (Logical s) scope -> ST s (TypeF (Logical s) scope)
fresh kind = do
  box <- newSTRef $! Unsolved {kind, constraints = Map.empty, erasure = Mask.Erased}
  pure $ Logical $ Box box

unify :: forall s scope. Context s scope -> Position -> TypeF (Logical s) scope -> TypeF (Logical s) scope -> ST s ()
unify context_ position term1_ term2_ = unifyWith context_ term1_ term2_
  where
    unifyWith ::
      forall scope.
      Context s scope ->
      TypeF (Logical s) scope ->
      TypeF (Logical s) scope ->
      ST s ()
    unifyWith context = unify
      where
        unify ::
          TypeF (Logical s) scope ->
          TypeF (Logical s) scope ->
          ST s ()
        unify (Logical (Box reference)) (Logical (Box reference'))
          | reference == reference' = pure ()
          | otherwise = do
              box <- readSTRef reference
              box' <- readSTRef reference'
              combine box box'
          where
            combine (Solved term) (Solved term') = unify term term'
            combine (Solved (Logical logical)) Unsolved {} = do
              unify (Logical logical) (Logical (Box reference'))
            combine Unsolved {} (Solved (Logical logical')) = do
              unify (Logical (Box reference)) (Logical logical')
            combine (Solved term) Unsolved {kind, constraints, erasure} = do
              occurs context position erasure reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $! Solved term
            combine Unsolved {kind, constraints, erasure} (Solved term') = do
              occurs context position erasure reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $! Solved term'
            combine
              Unsolved {kind = kind1, constraints = constraints1, erasure = erasure1}
              Unsolved {kind = kind2, constraints = constraints2, erasure = erasure2}
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
                      Semantic.Unify.Type.unify context position kind1 kind2
                      constraints <- sequence $ Map.unionWith merge constraints1 constraints2
                      writeSTRef reference' $! Unsolved {kind = kind1, constraints, erasure = erasure1 <> erasure2}
                      writeSTRef reference $! Solved (Logical (Box reference'))
        unify (Logical (Box reference)) term' =
          readSTRef reference >>= \case
            Unsolved {kind, constraints, erasure} -> do
              occurs context position erasure reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $ Solved term'
            Solved term -> unify term term'
        unify term (Logical (Box reference')) =
          readSTRef reference' >>= \case
            Unsolved {kind, constraints, erasure} -> do
              occurs context position erasure reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $ Solved term
            Solved term' -> unify term term'
        -- Types involving a higher scope must be solved in said scope
        unify (Logical (Shift logical)) term' = do
          term' <- unshift context position term'
          unifyWith (Shift.unshift context) (Logical logical) term'
        unify term (Logical (Shift logical)) = do
          term <- unshift context position term
          unifyWith (Shift.unshift context) term (Logical logical)
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
        unify Levity Levity = pure ()
        unify _ _ = mismatch

    mismatch :: ST s ()
    mismatch = abort position (Unify context_ term1_ term2_)

-- todo merge this with Semantic.Check.Temporary.Type checking somehow
typeCheck :: Context s scope -> Position -> TypeF (Logical s) scope -> TypeF (Logical s) scope -> ST s ()
typeCheck context_ position = typeCheckWith context_
  where
    typeCheckWith :: Context s scope -> TypeF (Logical s) scope -> TypeF (Logical s) scope -> ST s ()
    typeCheckWith context@Context {localEnvironment, typeEnvironment} = typeCheck
      where
        typeCheck kind = \case
          Logical (Box reference) ->
            readSTRef reference >>= \case
              Solved typex -> typeCheck kind typex
              Unsolved {kind = kind'} -> do
                unify context position kind kind'
          Logical (Shift logical) -> do
            kind <- unshift context position kind
            typeCheckWith (Shift.unshift context) kind (Logical logical)
          Variable index -> case localEnvironment Local.Table.! index of
            Local.Rigid {rigid} -> do
              unify context position kind (runTypex $ Simple.lift rigid)
            Local.Wobbly {wobbly = Typex wobbly} -> do
              unify context position kind wobbly
          Constructor constructor -> do
            Typex kind' <- Builtin.kind (pure . Simple.lift) indexType indexLift constructor
            unify context position kind' kind
            where
              indexType index =
                case typeEnvironment Type.Table.! index of
                  TypeBinding {kind} -> do
                    kind <- kind
                    case kind of
                      TypeBinding.Rigid kind -> pure $ Simple.lift kind
                      TypeBinding.Wobbly kind -> pure kind
              indexLift constructor@Constructor.Index {typeIndex} = do
                datax <- do
                  let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
                  datax <- Builtin.index pure get typeIndex
                  Simple.Data.instanciate context position datax
                pure $ DataInstance.constructorFunction datax constructor
          Constraint -> unify context position (Type Large) kind
          Levity -> unify context position (Type Large) kind
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

-- |
-- Marking a type with `Known` forces a type to not be erased so that the
-- runtime can know what it is. `mark` only forces the head normal form of a
-- type to be known. So `mark (List a)` will only force `List` to be known, not
-- `a`.
--
-- This is done to allow type class variable to not be counted as runtime
-- variables in type class default methods.
mark :: forall s scope. Context s scope -> Position -> Mask.Erasure -> TypeF (Logical s) scope -> ST s ()
mark context position erasure term_ = mark context term_
  where
    mark :: Context s scope' -> TypeF (Logical s) scope' -> ST s ()
    mark context@Context {localEnvironment} = \case
      Logical (Box reference')
        | otherwise ->
            readSTRef reference' >>= \case
              Unsolved {kind, constraints, erasure = erasure'} -> do
                writeSTRef reference' Unsolved {kind, constraints, erasure = erasure <> erasure'}
              Solved typex -> mark context typex
      Logical (Shift logical) -> do
        mark (Shift.unshift context) (Logical logical)
      Variable index -> case localEnvironment Local.Table.! index of
        Local.Rigid {mask}
          | Mask.valid mask erasure -> pure ()
          | otherwise -> mismask
        Local.Wobbly {} -> case erasure of
          Mask.Erased -> pure ()
          Mask.Known -> error "runtime mask on wobbly"
      Constructor _ -> pure ()
      Call term1 _ -> mark context term1
      Function {} -> pure ()
      Type {} -> pure ()
      Constraint -> pure ()
      Small -> pure ()
      Large -> pure ()
      Universe -> pure ()
      Levity -> pure ()
    mismask = abort position (Mismask context term_)

occurs ::
  Context s scope ->
  Position ->
  Mask.Erasure ->
  STRef s (Box s scope) ->
  TypeF (Logical s) scope ->
  ST s ()
occurs context position erasure reference term_ = do
  occurs term_
  mark context position erasure term_
  where
    occurs = \case
      Logical (Box reference')
        | reference == reference' -> occurrenece
        | otherwise ->
            readSTRef reference' >>= \case
              Unsolved {} -> pure ()
              Solved typex -> occurs typex
      Logical Shift {} -> pure ()
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
      Levity -> pure ()
    occurrenece = abort position (Occurs context reference term_)

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  TypeF (Logical s) scope ->
  ST s (EvidenceF (Evidence.Logical s) scope)
constrain context position classx term = constrainWith context position classx term []

constrainWith ::
  forall s scope.
  Context s scope ->
  Position ->
  Type2.Index scope ->
  TypeF (Logical s) scope ->
  [TypeF (Logical s) scope] ->
  ST s (EvidenceF (Evidence.Logical s) scope)
constrainWith context_ position classx_ term_ arguments_ = constrainWith context_ classx_ term_ arguments_
  where
    constrainWith ::
      forall scope.
      Context s scope ->
      Type2.Index scope ->
      TypeF (Logical s) scope ->
      [TypeF (Logical s) scope] ->
      ST s (EvidenceF (Evidence.Logical s) scope)
    constrainWith context@Context {typeEnvironment} classx term@(Logical (Box reference)) arguments =
      readSTRef reference >>= \case
        Solved term -> constrainWith context classx term arguments
        Unsolved {kind, constraints, erasure} -> do
          case Map.lookup classx constraints of
            Nothing -> do
              target <- fresh (Type Large)
              let indexType index =
                    case typeEnvironment Type.Table.! index of
                      TypeBinding {kind} -> do
                        kind <- kind
                        case kind of
                          TypeBinding.Rigid kind -> pure $ Simple.lift kind
                          TypeBinding.Wobbly wobbly -> pure wobbly
                  indexLift constructor@Constructor.Index {typeIndex} = do
                    datax <- do
                      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
                      datax <- Builtin.index pure get typeIndex
                      Simple.Data.instanciate context position datax
                    pure $ DataInstance.constructorFunction datax constructor
              Typex real <- Builtin.kind (pure . Simple.lift) indexType indexLift classx
              unify context position (Function target Constraint) real

              typeCheck context position target (foldl Call term arguments)
              logical <- newSTRef Evidence.Unsolved {}
              let delay = Delay {arguments, evidence = Evidence.Logical (Evidence.Box logical)}
              writeSTRef reference $! Unsolved {kind, constraints = Map.insert classx delay constraints, erasure}
              pure $ Evidence.Logical (Evidence.Box logical)
            Just Delay {arguments = arguments', evidence}
              | length arguments == length arguments' -> do
                  zipWithM_ (unify context position) arguments arguments'
                  pure evidence
              | otherwise -> error "error argument length doesn't match"
    constrainWith context classx (Logical (Shift logical)) arguments = do
      arguments <- traverse (unshift context position) arguments
      let quit = abort position $ Unshift context (Constructor classx)
      classx <- Shift.partialUnshift quit classx
      shift <$> constrainWith (Shift.unshift context) classx (Logical logical) arguments
    constrainWith context@Context {localEnvironment} classx (Variable index) arguments
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just Local.Constraint {arguments = arguments', evidence} <- Map.lookup classx constraints,
        arguments' <- [runTypex $ Simple.lift argument | argument <- toList arguments'],
        length arguments' == length arguments = do
          traverse_ (uncurry $ unify context position) (zip arguments arguments')
          pure (runEvidencex $ Simple.Evidence.lift evidence)
    constrainWith context@Context {typeEnvironment} (Type2.Index classx) (Constructor index) arguments
      | TypeBinding {classInstances} <- typeEnvironment Type.Table.! classx,
        Just instancex <- Map.lookup index classInstances = do
          TypeBinding.Instance dependencies <- instancex
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let Typex argument =
                      Simple.instanciate
                        (Strict.Vector.fromList $ Typex <$> arguments)
                        (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable (Evidence.Class classx index) (Instanciation arguments)
            None -> pure $ Evidence.Variable (Evidence.Class classx index) Instanciation.Mono
    constrainWith context@Context {typeEnvironment} classx (Constructor (Type2.Index index)) arguments
      | TypeBinding {dataInstances} <- typeEnvironment Type.Table.! index,
        Just instancex <- Map.lookup classx dataInstances = do
          TypeBinding.Instance dependencies <- instancex
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let Typex argument =
                      Simple.instanciate
                        (Strict.Vector.fromList $ Typex <$> arguments)
                        (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable (Evidence.Data classx index) (Instanciation arguments)
            None -> pure $ Evidence.Variable (Evidence.Data classx index) Instanciation.Mono
    constrainWith context classx (Call function argument) arguments =
      constrainWith context classx function (argument : arguments)
    constrainWith context classx (Function argument result) arguments = do
      typeCheck context position (Type Small) argument
      typeCheck context position (Type Small) result
      constrainWith context classx (Constructor Type2.Arrow `Call` argument `Call` result) arguments
    constrainWith context classx typex arguments = case typex of
      Constructor typex -> Builtin.constrain quit constrain classx typex arguments
      _ -> quit
      where
        constrain classx typex = constrainWith context classx typex []
        quit = abort position (Constrain context_ classx_ term_ arguments_)

reconstrain ::
  Context s scope ->
  Position ->
  Map (Type2.Index scope) (Delay s scope) ->
  TypeF (Logical s) scope ->
  ST s ()
reconstrain context position constraints term =
  for_ (Map.toList constraints) $ \(classx, Delay {arguments, evidence}) -> do
    evidence' <- constrainWith context position classx term arguments
    Evidence.unify evidence evidence'

unshift ::
  forall s scope scopes.
  Context s (scope ':+ scopes) ->
  Position ->
  TypeF (Logical s) (scope ':+ scopes) ->
  ST s (TypeF (Logical s) scopes)
unshift context position typex = unshift typex
  where
    unshift = \case
      Logical (Box reference) ->
        readSTRef reference >>= \case
          Unsolved {kind, constraints, erasure} -> do
            let unshiftDelay (key, Delay {arguments, evidence}) = do
                  key <- Shift.partialUnshift misshift key
                  arguments <- traverse unshift arguments
                  evidence <- Evidence.unshift evidence
                  pure (key, Delay {arguments, evidence})
            kind <- unshift kind
            -- Type variables may link to constraints that mention the variable
            -- itself. To allow unshifting these cycles, the unshifted box is
            -- initialized with undefined then later let to it's proper value.
            box <- newSTRef $ error "unshift cycle"
            writeSTRef reference $! Solved $ Logical $ Shift $ Box box
            -- there's no Map.traverseKeysMonotonic
            constraints <- Map.fromAscList <$> traverse unshiftDelay (Map.toAscList constraints)
            writeSTRef box $! Unsolved {kind, constraints, erasure}
            pure $ Logical $ Box box
          Solved term -> unshift term
      Logical (Shift logical) -> pure (Logical logical)
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
      Levity -> pure Levity
    misshift = do
      abort position $ Unshift context typex

defaultFrom,
  defaultUniverse ::
    Position ->
    Map (Type2.Index scope) (Delay s scope) ->
    TypeF (Logical s) scope ->
    ST s (Simple.Type scope)
defaultFrom position constraints kind = case kind of
  Logical (Box reference) ->
    readSTRef reference >>= \case
      Solved kind -> defaultFrom position constraints kind
      Unsolved {} -> unsupportedFeatureConstraintedTypeDefaulting position
  Type universe -> defaultUniverse position constraints universe
  Levity -> pure $ Constructor Type2.Lazy
  _ -> unsupportedFeatureConstraintedTypeDefaulting position
defaultUniverse position constraints universe = case universe of
  Logical (Box reference) ->
    readSTRef reference >>= \case
      Solved kind -> defaultUniverse position constraints kind
      Unsolved {} -> pure $ Type Small
  Small
    | null constraints -> pure $ Constructor (Type2.Tuple 0)
    | otherwise -> unsupportedFeatureConstraintedTypeDefaulting position
  Large
    | null constraints -> pure $ Type Small
    | otherwise -> error "unexpected kind constraints"
  _ -> error "unexpected universe kind"

solve :: Position -> TypeF (Logical s) scope -> Solve s (Simple.Type scope)
solve position = Solve . solve
  where
    solve :: TypeF (Logical s) scope -> ST s (Simple.Type scope)
    solve = \case
      Logical (Box reference) ->
        readSTRef reference >>= \case
          Solved typex -> solve typex
          Unsolved {kind, constraints} -> defaultFrom position constraints kind
      Logical (Shift logical) -> shift <$> solve (Logical logical)
      Variable name -> pure $ Variable name
      Constructor index -> pure $ Constructor index
      Call function argument -> do
        function <- solve function
        argument <- solve argument
        pure $ Call function argument
      Function argument result -> do
        argument <- solve argument
        result <- solve result
        pure $ Function argument result
      Type universe -> do
        universe <- solve universe
        pure $ Type universe
      Constraint -> pure Constraint
      Small -> pure Small
      Large -> pure Large
      Universe -> pure Universe
      Levity -> pure Levity
