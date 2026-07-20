module Core.Tree.Type where

import qualified Core.Shift as Shift2
import qualified Core.Show as Core
import Core.Substitute (Category (..))
import qualified Core.Substitute as Substitute
import qualified Data.Vector as Vector
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Local (Index (Local, Shift))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Type as Solved

type Type = TypeF Vacuous

data TypeF logical scope
  = Logical !(logical scope)
  | Variable !(Local.Index scope)
  | Constructor !(Type2.Index scope)
  | Call !(TypeF logical scope) !(TypeF logical scope)
  | Function !(TypeF logical scope) !(TypeF logical scope)
  | Type !(TypeF logical scope)
  | Constraint
  | Small
  | Large
  | Universe
  | Levity

infixr 0 `Function`

infixl 9 `Call`

instance Core.Show TypeF where
  showsPrec = showsPrec

instance (Scope.Show logical) => Show (TypeF logical scope) where
  showsPrec p = \case
    Logical logical -> showParen (p > 10) $ showString "Logical " . Scope.showsPrec 11 logical
    Variable variable -> showParen (p > 10) $ showString "Variable " . showsPrec 11 variable
    Constructor index -> showParen (p > 10) $ showString "Constructor " . showsPrec 11 index
    Call function argument ->
      showParen (p > 10) $
        showString "Call "
          . showsPrec 11 function
          . showString " "
          . showsPrec 11 argument
    Function parameter result ->
      showParen (p > 10) $
        showString "Function "
          . showsPrec 11 parameter
          . showString " "
          . showsPrec 11 result
    Type universe -> showParen (p > 10) $ showString "Type " . showsPrec 11 universe
    Constraint -> showString "Constraint"
    Small -> showString "Small"
    Large -> showString "Large"
    Universe -> showString "Universe"
    Levity -> showString "Levity"

instance (Scope.Eq logical) => Eq (TypeF logical scope) where
  Logical logical1 == Logical logical2 = logical1 Scope.== logical2
  Variable variable1 == Variable variable2 = variable1 == variable2
  Constructor index1 == Constructor index2 = index1 == index2
  Call function1 argument1 == Call function2 argument2 =
    function1 == function2 && argument1 == argument2
  Function function1 argument1 == Function function2 argument2 =
    function1 == function2 && argument1 == argument2
  Type universe1 == Type universe2 = universe1 == universe2
  Constraint == Constraint = True
  Small == Small = True
  Large == Large = True
  Universe == Universe = True
  Levity == Levity = True
  _ == _ = False

smallType :: Type scope
smallType = Type Small

instance (Shift.Functor logical) => Shift0.Functor (TypeF logical) where
  map = Shift.mapDefault

instance (Shift.Functor logical) => Shift.Functor (TypeF logical) where
  map category typex = case typex of
    Logical logical -> Logical (Shift.map category logical)
    Variable index -> Variable (Shift.map category index)
    Constructor index -> Constructor (Shift.map category index)
    Call function argument -> Call (Shift.map category function) (Shift.map category argument)
    Function parameter result ->
      Function (Shift.map category parameter) (Shift.map category result)
    Type universe -> Type (Shift.map category universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

instance (Scope.Show logical) => Scope.Show (TypeF logical) where
  showsPrec = showsPrec

instance (logical ~ Vacuous) => Shift2.Functor (TypeF logical) where
  map = Substitute.mapDefault

instance (logical ~ Vacuous) => Substitute.Functor (TypeF logical) where
  map = Substitute.mapType

instance Substitute.TypeFunctor TypeF where
  mapType (Substitute lift replacements _) (Variable index) = case index of
    Local index -> replacements Vector.! index
    Shift index -> Variable (Shift.map lift index)
  mapType (Substitute.Lift category) (Variable index) = Variable $ Shift2.map category index
  mapType Substitute.Over {} (Variable (Local.Local index)) = Variable (Local.Local index)
  mapType (Substitute.Over category) (Variable (Local.Shift index)) =
    shift $ Substitute.mapType category (Variable index)
  mapType category typex = case typex of
    Constructor index -> Constructor (Shift2.map (Substitute.general category) index)
    Call function argument -> Call (Substitute.mapType category function) (Substitute.mapType category argument)
    Function parameter result ->
      Function (Substitute.mapType category parameter) (Substitute.mapType category result)
    Type universe -> Type (Substitute.mapType category universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

simplify :: Solved.Type position Check scope -> Type scope
simplify typex = simplifyWith typex []

simplifyWith :: Solved.Type position Check scope -> [Type scope] -> Type scope
simplifyWith Solved.Constructor {constructor, synonym} arguments = case synonym of
  Solved.Synonym synonym -> Substitute.map category synonym
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
