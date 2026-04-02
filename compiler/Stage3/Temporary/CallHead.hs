module Stage3.Temporary.CallHead where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Table.Term as Term ((!))
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Term as Term (Index)
import qualified Stage2.Tree.CallHead as Stage2
import qualified Stage3.Check.ClassInstance as ClassInstance
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (..))
import qualified Stage3.Check.DataInstance as DataInstance
import Stage3.Check.TermBinding (TermBinding (..), Type (..))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Class as Simple.Class
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.Scheme (instanciate)
import Stage3.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage3.Temporary.ConstructorInfo as ConstructorInfo
import Stage3.Temporary.SelectorInfo (SelectorInfo)
import qualified Stage3.Temporary.SelectorInfo as SelectorInfo
import qualified Stage3.Tree.CallHead as Solved
import Stage3.Tree.MethodInfo (MethodInfo)
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Builtin as Builtin
import Stage4.Tree.TypeDeclaration (assumeClass, assumeData)

data CallHead s scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Selector
      { selector :: !(Selector.Index scope),
        selectorInfo :: !(SelectorInfo s scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope),
        evidence :: !(Unify.Evidence s scope),
        instanciation :: !(Unify.Instanciation s scope),
        methodInfo :: !MethodInfo
      }

instance Unify.Zonk CallHead where
  zonk zonker = \case
    Variable {variablePosition, variable, instanciation} -> do
      instanciation <- Unify.zonk zonker instanciation
      pure Variable {variablePosition, variable, instanciation}
    Constructor {constructor, constructorInfo} -> do
      constructorInfo <- Unify.zonk zonker constructorInfo
      pure Constructor {constructor, constructorInfo}
    Selector {selector, selectorInfo} -> do
      selectorInfo <- Unify.zonk zonker selectorInfo
      pure Selector {selector, selectorInfo}
    Method {methodPosition, method, evidence, instanciation, methodInfo} -> do
      evidence <- Unify.zonk zonker evidence
      instanciation <- Unify.zonk zonker instanciation
      pure Method {methodPosition, method, evidence, instanciation, methodInfo}

check :: Context s scope -> Unify.Type s scope -> Stage2.CallHead scope -> ST s (CallHead s scope)
check context@Context {termEnvironment} typex Stage2.Variable {variablePosition, variable} = do
  let TermBinding binding = termEnvironment Term.! variable
  binding >>= \case
    Wobbly scheme -> do
      (typex', instanciation) <- Unify.instanciate context variablePosition (Unify.monoScheme scheme)
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
    pure Constructor {constructor, constructorInfo}
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
    pure Selector {selector, selectorInfo}
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
        { variable,
          instanciation
        }
  Constructor {constructor, constructorInfo} -> do
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure $ Solved.Constructor {constructor, constructorInfo}
  Selector {selector, selectorInfo} -> do
    selectorInfo <- SelectorInfo.solve selectorInfo
    pure $ Solved.Selector {selector, selectorInfo}
  Method {methodPosition, method, evidence, instanciation, methodInfo} -> do
    evidence <- Unify.solveEvidence methodPosition evidence
    instanciation <- Unify.solveInstanciation methodPosition instanciation
    pure $
      Solved.Method
        { method,
          evidence,
          instanciation,
          methodInfo
        }
