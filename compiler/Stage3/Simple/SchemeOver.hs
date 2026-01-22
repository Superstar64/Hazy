module Stage3.Simple.SchemeOver
  ( Lift (..),
    lift,
    augmentNamed,
    augment,
    augment',
  )
where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Kind
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Order (orderWithInt)
import Stage1.Lexer (variableIdentifier)
import Stage1.Position (Position)
import Stage1.Variable (VariableIdentifier)
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type (Table (..), (!))
import qualified Stage2.Label.Binding.Local as Label
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.LocalBinding as LocalBinding
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Index.Evidence as Evidence (assumed)
import qualified Stage3.Simple.Constraint as Constraint (lift)
import qualified Stage3.Simple.Type as Type (lift)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} qualified Stage4.Tree.Class as Class (Class (..))
import Stage4.Tree.Constraint (Constraint (..))
import qualified Stage4.Tree.Evidence as Evidence (Evidence (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import Stage4.Tree.Type (Type)
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration (assumeClass)
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
    (fmap Constraint.lift constraints)
    (liftResult result)

augmentNamed ::
  (Int -> VariableIdentifier) ->
  Position ->
  Strict.Vector (Type scope) ->
  Strict.Vector (Constraint scope) ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augmentNamed name position parameters constraints Context {termEnvironment, localEnvironment, typeEnvironment}
  | parameters <- Strict.Vector.toLazy parameters = do
      let entailed classx arguments evidence = do
            Class.Class {constraints} <- do
              let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get classx
            children <- for (zip [0 ..] $ toList constraints) $ \(index, Constraint {classx, arguments = arguments'}) ->
              entailed classx (arguments <> arguments') Evidence.Super {base = evidence, index}
            let root =
                  LocalBinding.Constraint
                    { arguments,
                      evidence
                    }
            pure $ (shift classx, root) : concat children
          collect Constraint {classx, head, arguments} = do
            let evidence = Evidence.Proof {proof = Evidence.assumed head, arguments = Strict.Vector.empty}
            entail <- entailed classx arguments evidence
            pure (head, entail)
      collected <- traverse collect (toList constraints)
      let ordered = orderWithInt (++) [] (length parameters) collected
          constraints = Vector.map (Map.fromListWith $ LocalBinding.combine position) ordered
          rigid index typex constraints =
            LocalBinding.Rigid
              { label = Label.LocalBinding {name = name index},
                rigid = shift typex,
                constraints
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
  Strict.Vector.Vector (Constraint scope) ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment = augmentNamed name
  where
    name i = variableIdentifier $ pack $ "__rigid_" ++ show i

augment' ::
  Position ->
  SchemeOver typex scope ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment' position SchemeOver {parameters, constraints} =
  augment position parameters constraints
