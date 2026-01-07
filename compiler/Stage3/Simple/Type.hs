module Stage3.Simple.Type where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Index.Local (Index (Local, Shift))
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Type as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Prelude hiding (map)

data Type scope
  = Variable !(Local.Index scope)
  | Constructor !(Type2.Index scope)
  | Call !(Type scope) !(Type scope)
  | Function !(Type scope) !(Type scope)
  | Type !(Type scope)
  | Constraint
  | Small
  | Large
  | Universe
  deriving (Show)

infixr 0 `Function`

infixl 9 `Call`

smallType = Type Small

instance Shift Type where
  shift = shiftDefault

instance Shift.Functor Type where
  map category = map (Shift.map category) (Shift.map category)
    where
      map variable constructor =
        substitute (Variable . variable) (Constructor . Type2.map constructor)

instance Scope.Show Type where
  showsPrec = showsPrec

substitute ::
  (Local.Index scope1 -> Type scope2) ->
  (Type2.Index scope1 -> Type scope2) ->
  Type scope1 ->
  Type scope2
substitute variable constructor = \case
  Variable index -> variable index
  Constructor index -> constructor index
  Call function argument ->
    Call
      (substitute variable constructor function)
      (substitute variable constructor argument)
  Function argument result ->
    Function
      (substitute variable constructor argument)
      (substitute variable constructor result)
  Type universe -> Type (substitute variable constructor universe)
  Constraint -> Constraint
  Small -> Small
  Large -> Large
  Universe -> Universe

instanciate :: Strict.Vector (Unify.Type s scope) -> Type (Local ':+ scope) -> Unify.Type s scope
instanciate fresh = \case
  Variable index -> case index of
    Local.Shift index -> Unify.variable index
    Local.Local index -> fresh Strict.Vector.! index
  Constructor index -> Unify.constructor (Type2.map Type.unlocal index)
  Call function argument -> instanciate fresh function `Unify.call` instanciate fresh argument
  Function argument result -> instanciate fresh argument `Unify.function` instanciate fresh result
  Type universe -> Unify.typeWith (instanciate fresh universe)
  Constraint -> Unify.constraint
  Small -> Unify.small
  Large -> Unify.large
  Universe -> Unify.universe

-- todo remove this ugly hack

instanciate' ::
  Strict.Vector (Unify.Type s scope) ->
  Type (Local ':+ Local ':+ scope) ->
  Unify.Type s (Local ':+ scope)
instanciate' fresh = \case
  Variable index -> case index of
    Local.Shift (Local.Shift index) -> Unify.variable (Local.Shift index)
    Local.Shift (Local.Local index) -> shift $ fresh Strict.Vector.! index
    Local.Local index -> Unify.variable (Local.Local index)
  Constructor index -> Unify.constructor (Type2.map Type.unlocal index)
  Call function argument -> instanciate' fresh function `Unify.call` instanciate' fresh argument
  Function argument result -> instanciate' fresh argument `Unify.function` instanciate' fresh result
  Type universe -> Unify.typeWith (instanciate' fresh universe)
  Constraint -> Unify.constraint
  Small -> Unify.small
  Large -> Unify.large
  Universe -> Unify.universe

lift :: Type scope -> Unify.Type s scope
lift = instanciate undefined . shift

simplify :: Solved.Type scope -> Type scope
simplify typex = simplifyWith typex []

simplifyWith :: Solved.Type scope -> [Type scope] -> Type scope
simplifyWith Solved.Constructor {Solved.constructor, Solved.synonym} arguments = case synonym of
  Strict.Just synonym -> replace (Vector.fromList arguments) synonym
    where
      replace :: Vector (Type scope) -> Type (Local ':+ scope) -> Type scope
      replace replacements = substitute replace (Constructor . Type2.map Type.unlocal)
        where
          replace = \case
            Local index -> replacements Vector.! index
            Shift index -> Variable index
  Strict.Nothing -> foldl Call (Constructor constructor) arguments
simplifyWith Solved.Call {Solved.function, Solved.argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith typex arguments@(_ : _) =
  foldl Call (simplify typex) arguments
simplifyWith typex [] = case typex of
  Solved.Variable {Solved.variable} -> Variable variable
  Solved.Tuple {Solved.elements} ->
    foldl Call (Constructor $ Type2.Tuple (length elements)) (fmap simplify elements)
  Solved.Function {Solved.parameter, Solved.result} ->
    Function (simplify parameter) (simplify result)
  Solved.List {Solved.element} -> Constructor Type2.List `Call` simplify element
  Solved.LiftedList {Solved.items} ->
    let nil = Constructor (Type2.Lifted Constructor.nil)
        cons head tail =
          Constructor (Type2.Lifted Constructor.cons) `Call` head `Call` tail
     in foldr (cons . simplify) nil items
  Solved.SmallType {} -> Type Small
  Solved.Constraint {} -> Constraint
