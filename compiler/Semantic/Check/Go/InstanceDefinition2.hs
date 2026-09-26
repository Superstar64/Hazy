module Semantic.Check.Go.InstanceDefinition2 where

import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Substitute (Category (Substitute), logicalType)
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Class as Core.Class
import Core.Tree.ClassExtra (ClassExtra (..))
import qualified Core.Tree.Constraint as Core (ConstraintF (..))
import qualified Core.Tree.Data as Core (Data)
import qualified Core.Tree.Evidence as Core (Evidence)
import qualified Core.Tree.Evidence as Core.Evidence
import Core.Tree.EvidenceSet (EvidenceSet (..))
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Forall as Core (ForallOver (..))
import qualified Core.Tree.Instanciation as Core (InstanciationF (Instanciation, Mono))
import qualified Core.Tree.Instanciation as Core.Instanciation
import Core.Tree.Type ((#))
import qualified Core.Tree.Type as Core (Type, TypeF, typeWith, universe)
import qualified Core.Tree.Type as Core.Type
import Core.Tree.TypeDeclaration (assumeClass, assumeData)
import qualified Core.Tree.TypeDeclarationExtra as Extra
import qualified Core.Tree.TypeLambda as Core (TypeLambdaOver (..))
import Data.Functor.Identity (Identity (..))
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cannotDerive, cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.Derive.Eq as Eq
import Semantic.Check.Go.Definition4 (Solve (..))
import qualified Semantic.Check.Go.Scheme as Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Scheme as Core.Scheme
import qualified Semantic.Check.Temporary.Constraints as Unsolved.Constraints (check, solve)
import qualified Semantic.Check.Temporary.Definition as Definition
import qualified Semantic.Check.Temporary.Scheme as Unsolved (augment)
import qualified Semantic.Check.Temporary.TypePattern as Unsolved (TypePattern (..), solve)
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type as Type (Index)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (Resolve))
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Constraints (Constraints (Constraints, None))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (Annotation (..), Header (..), InstanceDefinition2 (..))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (..))
import qualified Semantic.Tree.Type as Type (Synonym (..), Type (..), label)
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import qualified Syntax.Printer as Printer
import qualified Syntax.Tree.Type as Syntax.Type
import Prelude hiding (head)

data Key scope
  = Data
      { index1 :: !(Type2.Index scope),
        head1 :: !(Type.Index scope)
      }
  | Class
      { index2 :: !(Type.Index scope),
        head2 :: !(Type2.Index scope)
      }

keyIndex :: Key scope -> Type2.Index scope
keyIndex = \case
  Data {index1} -> index1
  Class {index2} -> Type2.Index index2

keyHead :: Key scope -> Type2.Index scope
keyHead = \case
  Data {head1} -> Type2.Index head1
  Class {head2} -> head2

checkHeader ::
  Position ->
  (f (ST s) -> Context s scope) ->
  Header Resolve scope ->
  Topological.Formula f s (Header Check scope)
checkHeader position information Header {parameters, prerequisites} =
  Topological.Formula
    { cycle = cyclicalTypeChecking position,
      run = \declarations -> do
        let context = information declarations
            fresh TypePattern {name, position} = do
              level <- Unify.fresh Core.universe
              typex <- Unify.fresh (Core.typeWith level)
              pure
                Unsolved.TypePattern
                  { name,
                    typex,
                    position
                  }
        parameters <- traverse fresh parameters
        context <- pure $ Unsolved.augment parameters context
        prerequisites <- Unsolved.Constraints.check context prerequisites

        parameters <- Unify.runSolve $ traverse Unsolved.solve parameters
        prerequisites <- Unify.runSolve $ Unsolved.Constraints.solve context prerequisites
        pure Header {parameters, prerequisites}
    }

data Source origin scope where
  Manual :: Source origin scope
  -- `Core.Data` may be undefined. This, however, should never happen for
  -- `deriving` clauses that are not orphans.
  Derive ::
    !(Type2.Index scope) ->
    !(Type2.Index scope) ->
    Core.Data scope ->
    Source Auto scope

data Method s scope = Method
  { position :: Position,
    context :: Context s (Local ':+ scope),
    base :: Core.Type (Local ':+ scope),
    self :: Core.Evidence (Local ':+ scope),
    extra :: Unify.Solve s (ClassExtra scope)
  }

checkMethod ::
  Method s scope ->
  Source origin scope ->
  Int ->
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  MethodConcrete origin Group Resolve (Local ':+ scope) ->
  ST s (Unify.Solve s (MethodConcrete origin Group Check scope))
checkMethod Method {position, context} Manual _ scheme (Definition (Resolve member)) = do
  let Core.ForallOver {parameters, constraints, result} = scheme
  result <- pure $ logicalType result
  context <- Core.Scheme.augmentForall position scheme Mask.Runtime context
  definition <- Definition.check context result member
  pure $ do
    result <- Definition.solve definition
    pure $
      Definition $
        Implicit.Check
          Core.TypeLambdaOver
            { parameters,
              constraints,
              result
            }
checkMethod Method {context, position} (Derive Type2.Eq typeIndex datax) index _ _
  | Method.Equal <- toEnum index = Eq.equal context position typeIndex datax
checkMethod Method {context, position, base, self, extra} source index scheme Generated {} = do
  case source of
    Manual -> pure ()
    Derive classx datax _ -> case classx of
      Type2.Eq -> pure ()
      _ ->
        let labelContext = Context.label context
            resolve =
              Type.Call
                { startPosition = (),
                  function =
                    Type.Constructor
                      { startPosition = (),
                        constructorPosition = (),
                        constructor = shift classx,
                        synonym = Type.NoSynonym
                      },
                  argument =
                    Type.Constructor
                      { startPosition = (),
                        constructorPosition = (),
                        constructor = shift datax,
                        synonym = Type.NoSynonym
                      }
                }
            syntax = Type.label labelContext resolve
            text = Printer.build $ Syntax.Type.print syntax
         in cannotDerive position text
  pure $ do
    let Core.ForallOver {parameters, constraints} = scheme
    defaultx <- do
      ClassExtra {defaults} <- extra
      pure $
        Core.TypeLambdaOver
          { parameters,
            constraints,
            result = defaults Strict.Vector.! index
          }
    let typeReplacements = Vector.singleton base
        evidenceReplacements = Vector.singleton self
        category = Substitute Shift.Shift typeReplacements evidenceReplacements
    pure $ Generated $ Solved $ Substitute.map category defaultx

checkBody ::
  (Monad solve) =>
  Position ->
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> InstanceDefinition2 solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  (Context s scope -> Type2.Index scope -> Type2.Index scope -> ST s (Source origin scope)) ->
  Strict.Vector (MethodConcrete origin Group Resolve scope) ->
  Topological.Formula declarations s (solve (InstanceDefinition origin Group Check scope))
checkBody position key Solve {solve} reflection information lookup members =
  Topological.Formula
    { cycle = cyclicalTypeChecking position,
      run = \declarations -> do
        let context@Context {typeEnvironment} = information declarations
            index = keyIndex key
            head = keyHead key
        Header {parameters, prerequisites} <- case reflection declarations of
          Standard header ::: _ -> header
          DerivedInstance header ::: _ -> header
        Core.Class.Class {constraints, methods} <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
          Builtin.index pure get index
        extra <- do
          let get index = do
                extra <- TypeBinding.extra (typeEnvironment Table.Type.! index)
                pure (Extra.assumeClass <$> extra)
          Builtin.index (pure . pure) get index
        source <- lookup context index head
        let base = foldl (#) (shift $ Core.Type.Constructor head) variables
              where
                variables = [Core.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
            self = Core.Evidence.Variable {variable = shift variable, instanciation}
              where
                variable = case key of
                  Data {index1, head1} -> Evidence.Data index1 head1
                  Class {index2, head2} -> Evidence.Class index2 head2
                instanciation = case prerequisites of
                  Semantic.Tree.Constraints.None -> Core.Mono
                  Constraints prerequisites -> Core.Instanciation $ Strict.Vector.fromList $ do
                    i <- [0 .. length prerequisites - 1]
                    pure
                      Core.Evidence.Variable
                        { variable = Evidence.Index (Evidence0.Assumed i),
                          instanciation = Core.Instanciation.Mono
                        }
        context <- Scheme.augment position parameters prerequisites Mask.Runtime context
        {-
          instances where the constraints contain the variable in the non head part
          should not kind check, so we should be able to safely ignore them
          For example:
          > instance (F (a a)) => G (H a)
        -}
        methods <-
          pure $
            let replacements = Vector.singleton base
                category = Substitute.Over $ Substitute Shift.Shift replacements (error "no evidence")
                substitute Core.ForallOver {parameters, constraints, result} =
                  Core.ForallOver
                    { parameters,
                      constraints,
                      result = Substitute.map category result
                    }
             in substitute <$> methods
        evidence <- for constraints $
          \Core.Constraint {classx, arguments} -> do
            let parameter = foldl (#) base arguments
            evidence <- Unify.constrain context position (shift classx) (logicalType parameter)
            Unify.runSolve $ Unify.solveEvidence position evidence
        let arguments =
              Method
                { position,
                  context,
                  base,
                  self,
                  extra
                }
        members <- Strict.Vector.izipWithM (checkMethod arguments source) methods (shift <$> members)
        members <- solve $ sequence members
        pure $ do
          members <- members
          pure
            InstanceDefinition
              { evidence = Solved $ EvidenceSet $ evidence,
                members
              }
    }

check ::
  (Monad solve) =>
  Position ->
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> InstanceDefinition2 solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  InstanceDefinition2 Identity Identity Group Resolve scope ->
  InstanceDefinition2 solve (Topological.Formula declarations s) Group Check scope
check position key solve reflection information = \case
  Standard (Identity header) ::: Identity (Identity InstanceDefinition {members}) ->
    Standard (checkHeader position information header)
      ::: checkBody position key solve reflection information lookup members
    where
      lookup _ _ _ = pure Manual
  DerivedInstance (Identity header) ::: Identity (Identity InstanceDefinition {members}) ->
    DerivedInstance (checkHeader position information header)
      ::: checkBody position key solve reflection information lookup members
    where
      lookup Context {typeEnvironment} index (Type2.Index head) = do
        datax <- assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! head)
        pure $ Derive index (Type2.Index head) datax
      lookup _ index head = pure $ Derive index head $ error "not user defined data"

solve ::
  InstanceDefinition2 (Unify.Solve s) Identity Group Check scope ->
  Unify.Solve s (InstanceDefinition2 Identity Identity Group Check scope)
solve (annotation ::: Identity definition) = do
  definition <- definition
  pure $ annotation ::: Identity (Identity definition)
