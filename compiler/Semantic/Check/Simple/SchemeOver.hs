module Semantic.Check.Simple.SchemeOver
  ( Lift (..),
    lift,
    augmentNamed,
    augment,
    augment',
  )
where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Builtin as Builtin
import {-# SOURCE #-} qualified Core.Tree.Class as Class (Class (..))
import Core.Tree.Constraint (Constraint (..))
import Core.Tree.Constraints (Constraints (..))
import qualified Core.Tree.Evidence as Evidence (Evidence (..))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.SchemeOver (SchemeOver (..))
import Core.Tree.Type (Type)
import {-# SOURCE #-} Core.Tree.TypeDeclaration (assumeClass)
import Data.Foldable (toList)
import qualified Data.Kind
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Order (orderWithInt)
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.LocalBinding as LocalBinding
import Semantic.Check.Mask (Mask)
import qualified Semantic.Check.Simple.Constraints as Constraints
import qualified Semantic.Check.Simple.Type as Type (lift)
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Evidence as Evidence (assumed)
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type (Table (..), (!))
import qualified Semantic.Label.Binding.Local as Label
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (shift)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Lexer (variableIdentifier)
import Syntax.Position (Position)
import Syntax.Variable (VariableIdentifier)
import Prelude hiding (head)

type Lift ::
  (Environment -> Data.Kind.Type) ->
  (Data.Kind.Type -> Environment -> Data.Kind.Type) ->
  Data.Kind.Type ->
  Data.Kind.Type
newtype Lift typex typex' s = Lift (forall scope. typex scope -> typex' s scope)

lift :: Lift typex typex' s -> SchemeOver typex scope -> Unify.SchemeOver typex' s scope
lift (Lift liftResult) SchemeOver {parameters, constraints, result} =
  Unify.schemeOver
    (fmap Type.lift parameters)
    (Constraints.lift constraints)
    (liftResult result)

augmentNamed ::
  (Int -> VariableIdentifier) ->
  Position ->
  Strict.Vector (Type scope) ->
  Constraints scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augmentNamed name position parameters constraints mask Context {termEnvironment, localEnvironment, typeEnvironment}
  | parameters <- Strict.Vector.toLazy parameters = do
      let entailed classx arguments evidence = do
            Class.Class {constraints} <- do
              let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get classx
            children <- for (zip [0 ..] $ toList constraints) $
              \(index, Constraint {classx, arguments = arguments'}) ->
                entailed classx (arguments <> arguments') Evidence.Super {base = evidence, index}
            let root =
                  LocalBinding.Constraint
                    { arguments,
                      evidence
                    }
            pure $ (shift classx, root) : concat children
          collect index Constraint {classx, head, arguments} = do
            let evidence =
                  Evidence.Variable
                    { variable = Evidence.assumed index,
                      instanciation = Instanciation.Mono
                    }
            entail <- entailed classx arguments evidence
            pure (head, entail)
      collected <- case constraints of
        Constraints constraints -> zipWithM collect [0 ..] $ toList constraints
        None -> pure []
      let ordered = orderWithInt (++) [] (length parameters) collected
          constraints = Vector.map (Map.fromListWith $ LocalBinding.combine position) ordered
          rigid index typex constraints =
            LocalBinding.Rigid
              { label = Label.LocalBinding {name = name index},
                rigid = shift typex,
                constraints,
                mask
              }
          locals = Vector.izipWith rigid parameters constraints
      () <- pure $ foldr (\map () -> foldr seq () map) () constraints
      pure
        Context
          { termEnvironment = Term.Local termEnvironment,
            localEnvironment = Local.Local locals localEnvironment,
            typeEnvironment = Type.Local typeEnvironment
          }

augment ::
  Position ->
  Strict.Vector.Vector (Type scope) ->
  Constraints scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment = augmentNamed name
  where
    name i = variableIdentifier $ pack $ "__rigid_" ++ show i

augment' ::
  Position ->
  SchemeOver typex scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment' position SchemeOver {parameters, constraints} =
  augment position parameters constraints
