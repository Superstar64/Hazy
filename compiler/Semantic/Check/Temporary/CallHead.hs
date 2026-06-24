module Semantic.Check.Temporary.CallHead where

import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Tree.TypeDeclaration (assumeClass, assumeData)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.ClassInstance as ClassInstance
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.DataInstance (DataInstance (..))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Class as Simple.Class
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Simple.MethodInfo (MethodInfo)
import Semantic.Check.Simple.Scheme (instanciate)
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import Semantic.Check.Temporary.SelectorInfo (SelectorInfo)
import qualified Semantic.Check.Temporary.SelectorInfo as SelectorInfo
import Semantic.Check.TermBinding (TermBinding (..), Type (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Table.Term as Term ((!))
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Index.Term as Term (Index)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.CallHead as Semantic
import qualified Semantic.Tree.CallHead as Solved
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data CallHead s scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Selector
      { selectorPosition :: !Position,
        selector :: !(Selector.Index scope),
        selectorInfo :: !(SelectorInfo s scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope),
        evidence :: !(Unify.Evidence s scope),
        instanciation :: !(Unify.Instanciation s scope),
        methodInfo :: !(MethodInfo scope)
      }

check :: Context s scope -> Unify.Type s scope -> Semantic.CallHead Resolve scope -> ST s (CallHead s scope)
check context@Context {termEnvironment} typex Semantic.Variable {variablePosition, variable} = do
  let TermBinding binding = termEnvironment Term.! variable
  binding >>= \case
    Wobbly scheme -> do
      (typex', instanciation) <- Unify.instanciate context variablePosition scheme
      Unify.unify context variablePosition typex typex'
      pure Variable {variablePosition, variable, instanciation}
    Rigid scheme -> do
      (typex', instanciation) <- instanciate context variablePosition scheme
      Unify.unify context variablePosition typex typex'
      pure Variable {variablePosition, variable, instanciation}
check context@Context {typeEnvironment} typex Semantic.Constructor {constructorPosition, constructor} =
  do
    let Constructor.Index typeIndex constructorIndex = constructor
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    DataInstance {types, constructors} <-
      Simple.Data.instanciate context constructorPosition datax
    let root = Unify.constructor typeIndex
        base = foldl Unify.call root types
        instancex = constructors Strict.Vector.! constructorIndex
        typex' = ConstructorInstance.function instancex base
        constructorInfo = ConstructorInstance.info instancex
    Unify.unify context constructorPosition typex typex'
    pure Constructor {constructorPosition, constructor, constructorInfo}
check
  context@Context {typeEnvironment}
  typex
  Semantic.Selector {selectorPosition, selector} = do
    let Selector.Index typeIndex selectorIndex = selector
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    instancex <- Simple.Data.instanciate context selectorPosition datax
    let selectorInfo = DataInstance.selectorInfo instancex selectorIndex
        typex' = DataInstance.selectorFunction instancex selector
    Unify.unify context selectorPosition typex typex'
    pure Selector {selectorPosition, selector, selectorInfo}
check
  context@Context {typeEnvironment}
  typex
  Semantic.Method {methodPosition, method} = do
    case method of
      Method.Index typeIndex methodIndex -> do
        classx <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        instancex <- Simple.Class.instanciate context methodPosition typeIndex classx
        let function = ClassInstance.methodFunction instancex methodIndex
            methodInfo = ClassInstance.info instancex
            evidence = ClassInstance.evidence instancex
        (typex', instanciation) <- Unify.instanciate context methodPosition function
        Unify.unify context methodPosition typex typex'
        pure Method {methodPosition, method, evidence, instanciation, methodInfo}

solve = \case
  Variable {variablePosition, variable, instanciation} -> do
    instanciation <- Unify.solveInstanciation variablePosition instanciation
    pure
      Solved.Variable
        { variablePosition,
          variable,
          instanciation = Solved instanciation
        }
  Constructor {constructorPosition, constructor, constructorInfo} -> do
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure $
      Solved.Constructor
        { constructorPosition,
          constructor,
          constructorInfo = Solved constructorInfo
        }
  Selector {selectorPosition, selector, selectorInfo} -> do
    selectorInfo <- SelectorInfo.solve selectorInfo
    pure $
      Solved.Selector
        { selectorPosition,
          selector,
          selectorInfo = Solved selectorInfo
        }
  Method {methodPosition, method, evidence, instanciation, methodInfo} -> do
    evidence <- Unify.solveEvidence methodPosition evidence
    instanciation <- Unify.solveInstanciation methodPosition instanciation
    pure $
      Solved.Method
        { methodPosition,
          method,
          evidence = Solved evidence,
          instanciation = Solved instanciation,
          methodInfo = Solved methodInfo
        }
