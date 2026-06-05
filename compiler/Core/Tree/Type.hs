module Core.Tree.Type where

import qualified Core.Shift as Shift2
import Core.Substitute (Category (..), map)
import qualified Core.Substitute as Substitute
import qualified Data.Vector as Vector
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Local (Index (Local, Shift))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Type as Solved
import Prelude hiding (Functor, map)

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
  | Levity
  deriving (Show, Eq)

infixr 0 `Function`

infixl 9 `Call`

smallType = Type Small

instance Shift Type where
  shift = shiftDefault

instance Shift.Functor Type where
  map = Shift2.mapDefault

instance Scope.Show Type where
  showsPrec = showsPrec

instance Shift2.Functor Type where
  map = Substitute.mapDefault

instance Substitute.Functor Type where
  map (Substitute lift replacements _) (Variable index) = case index of
    Local index -> replacements Vector.! index
    Shift index -> Variable (Shift.map lift index)
  map (Substitute.Lift category) (Variable index) = Variable $ Shift2.map category index
  map Substitute.Over {} (Variable (Local.Local index)) = Variable (Local.Local index)
  map (Substitute.Over category) (Variable (Local.Shift index)) = shift $ map category (Variable index)
  map category typex = case typex of
    Constructor index -> Constructor (map category index)
    Call function argument -> Call (map category function) (map category argument)
    Function parameter result ->
      Function (map category parameter) (map category result)
    Type universe -> Type (map category universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

simplify :: Solved.Type position Check scope -> Type scope
simplify typex = simplifyWith typex []

simplifyWith :: Solved.Type position Check scope -> [Type scope] -> Type scope
simplifyWith Solved.Constructor {constructor, synonym} arguments = case synonym of
  Solved.Synonym synonym -> map category synonym
    where
      category = Substitute Shift.Id (Vector.fromList arguments) (error "no evidence")
  Solved.NoSynonym -> foldl Call (Constructor constructor) arguments
simplifyWith Solved.Call {function, argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith typex arguments@(_ : _) =
  foldl Call (simplify typex) arguments
simplifyWith typex [] = case typex of
  Solved.Variable {variable} -> Variable variable
  Solved.Tuple {elements} ->
    foldl Call (Constructor $ Type2.Tuple (length elements)) (fmap simplify elements)
  Solved.Function {parameter, result} ->
    Function (simplify parameter) (simplify result)
  Solved.List {element} -> Constructor Type2.List `Call` simplify element
  Solved.LiftedList {items} ->
    let nil = Constructor (Type2.Lifted Constructor.nil)
        cons head tail =
          Constructor (Type2.Lifted Constructor.cons) `Call` head `Call` tail
     in foldr (cons . simplify) nil items
  Solved.SmallType {} -> Type Small
  Solved.Constraint {} -> Constraint
  Solved.Levity {} -> Levity
