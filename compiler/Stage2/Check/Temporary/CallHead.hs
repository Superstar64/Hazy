module Stage2.Check.Temporary.CallHead where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Table.Term as Term ((!))
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Term as Term (Index)
import Stage2.Stage (Resolve)
import qualified Stage2.Tree.CallHead as Solved
import qualified Stage2.Tree.CallHead as Stage2
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Check.ClassInstance as ClassInstance
import qualified Stage2.Check.ConstructorInstance as ConstructorInstance
import Stage2.Check.Context (Context (..))
import Stage2.Check.DataInstance (DataInstance (..))
import qualified Stage2.Check.DataInstance as DataInstance
import Stage2.Check.TermBinding (TermBinding (..), Type (..))
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Check.Simple.Class as Simple.Class
import qualified Stage2.Check.Simple.Data as Simple.Data
import Stage2.Check.Simple.MethodInfo (MethodInfo)
import Stage2.Check.Simple.Scheme (instanciate)
import Stage2.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage2.Check.Temporary.ConstructorInfo as ConstructorInfo
import Stage2.Check.Temporary.SelectorInfo (SelectorInfo)
import qualified Stage2.Check.Temporary.SelectorInfo as SelectorInfo
import qualified Stage2.Unify as Unify
import qualified Stage4.Tree.Builtin as Builtin
import Stage4.Tree.TypeDeclaration (assumeClass, assumeData)

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

check :: Context s scope -> Unify.Type s scope -> Stage2.CallHead Resolve scope -> ST s (CallHead s scope)
check context@Context {termEnvironment} typex Stage2.Variable {variablePosition, variable} = do
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
check context@Context {typeEnvironment} typex Stage2.Constructor {constructorPosition, constructor} =
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
  Stage2.Selector {selectorPosition, selector} = do
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
  Stage2.Method {methodPosition, method} = do
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
