module Stage5.Generate.Context where

import Control.Monad.ST (ST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Stage2.Index.Table.Type as Type (Table (..), (!))
import qualified Stage2.Index.Type as Type (Index)
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import qualified Stage3.Index.Evidence0 as Evidence0 (Index)
import qualified Stage3.Index.Table.Evidence0 as Evidence0 (Table (..), (!))
import qualified Stage4.Index.Table.Term as Term (Table (..), (!))
import qualified Stage4.Index.Term as Term (Index)
import qualified Stage5.Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Stage5.Generate.Binding.Term as Term (Binding)
import qualified Stage5.Generate.Binding.Term as Term.Binding
import qualified Stage5.Generate.Binding.Type as Type (Binding (..))
import Stage5.Generate.Global (Global)
import Stage5.Generate.GlobalType (GlobalType (GlobalType))
import qualified Stage5.Generate.GlobalType as GlobalType
import Stage5.Generate.LocalType (LocalType (LocalType))
import qualified Stage5.Generate.LocalType as LocalType
import Stage5.Generate.Precontext (Precontext (Precontext))
import qualified Stage5.Generate.Precontext as Precontext

data Builtin = Builtin
  { numInt, numInteger, enumInt, enumInteger :: !Text
  }

data Context s scope = Context
  { terms :: !(Term.Table Term.Binding scope),
    evidence :: !(Evidence0.Table Evidence.Binding scope),
    types :: !(Type.Table Type.Binding scope),
    unique :: !(STRef s [Text]),
    used :: !(STRef s (Map Global Text)),
    builtin :: !Builtin
  }

start :: Precontext -> Builtin -> [Text] -> ST s (Context s Scope.Global)
start Precontext {terms, types} builtin unique = do
  unique <- newSTRef unique
  used <- newSTRef Map.empty
  let globalType GlobalType {classInstances, dataInstances} =
        Type.Binding
          { classInstances = Term.Binding.Global <$> classInstances,
            dataInstances = Term.Binding.Global <$> dataInstances
          }
  pure
    Context
      { terms = Term.Global (fmap Term.Binding.Global <$> terms),
        evidence = Evidence0.Global,
        types = Type.Global (fmap globalType <$> types),
        unique,
        used,
        builtin
      }

fresh :: Context s scope -> ST s Text
fresh Context {unique} = do
  list <- readSTRef unique
  writeSTRef unique $! tail list
  pure $ head list

localBindings ::
  Vector Text ->
  Vector (LocalType (Scope.Declaration ':+ scope)) ->
  Context s scope ->
  Context s (Scope.Declaration ':+ scope)
localBindings names instances Context {terms, evidence, types, unique, used, builtin} =
  Context
    { terms = Term.Declaration (Term.Binding.Local <$> names) terms,
      evidence = Evidence0.Declaration evidence,
      types = Type.Declaration (go <$> instances) types,
      unique,
      used,
      builtin
    }
  where
    go LocalType {classInstances, dataInstances} =
      Type.Binding
        { classInstances = Map.map Term.Binding.Local classInstances,
          dataInstances = Map.map Term.Binding.Local dataInstances
        }

patternBindings ::
  Vector Text ->
  Context s scope ->
  Context s (Scope.Pattern ':+ scope)
patternBindings names Context {terms, evidence, types, unique, used, builtin} =
  Context
    { terms = Term.Pattern (Term.Binding.Local <$> names) terms,
      evidence = Evidence0.Pattern evidence,
      types = Type.Pattern types,
      unique,
      used,
      builtin
    }

evidenceBindings ::
  Vector Text ->
  Context s scope ->
  Context s (Scope.Local ':+ scope)
evidenceBindings names Context {terms, evidence, types, unique, used, builtin} =
  Context
    { terms = Term.Local terms,
      evidence = Evidence0.Assumed (Evidence.Binding <$> names) evidence,
      types = Type.Local types,
      unique,
      used,
      builtin
    }

(!-) :: Context s scope -> Term.Index scope -> Term.Binding scope
Context {terms} !- index = terms Term.! index

(!=.) :: Context s scope -> Type.Index scope -> Type.Binding scope
Context {types} !=. index = types Type.! index

(!~) :: Context s scope -> Evidence0.Index scope -> Evidence.Binding scope
Context {evidence} !~ index = evidence Evidence0.! index

symbol :: Context s scope -> Term.Binding scope' -> ST s Text
symbol Context {used, unique} = \case
  Term.Binding.Local name -> pure name
  Term.Binding.Global global -> do
    known <- readSTRef used
    case Map.lookup global known of
      Just text -> pure text
      Nothing -> do
        list <- readSTRef unique
        let text = head list
        writeSTRef unique $! tail list
        writeSTRef used $! Map.insert global text known
        pure text
