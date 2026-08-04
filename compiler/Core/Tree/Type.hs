module Core.Tree.Type where

import Core.Substitute (Category (..))
import qualified Core.Substitute as Substitute
import qualified Core.Type.Functor as Core (Functor (..))
import qualified Core.Type.Show as Core (Show (..))
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Local (Index (Local, Shift))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Type as Solved

type Type = TypeF Void

data TypeF logical scope
  = Logical !logical
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

instance (Show logical) => Show (TypeF logical scope) where
  showsPrec p = \case
    Logical logical -> showParen (p > 10) $ showString "Logical " . showsPrec 11 logical
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

instance (Eq logical) => Eq (TypeF logical scope) where
  Logical logical1 == Logical logical2 = logical1 == logical2
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

instance Shift0.Functor (TypeF logical) where
  map = Shift.mapDefault

instance Shift.Functor (TypeF logical) where
  map category typex = case typex of
    Logical logical -> Logical logical
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

instance (Show logical) => Scope.Show (TypeF logical) where
  showsPrec = showsPrec

instance (logical ~ Void) => Substitute.Functor (TypeF logical) where
  map = Substitute.mapType

instance Substitute.TypeFunctor TypeF where
  mapType (Substitute lift replacements _) (Variable index) = case index of
    Local index -> replacements Vector.! index
    Shift index -> Variable (Shift.map lift index)
  mapType (Substitute.Lift category) (Variable index) = Variable $ Shift.map category index
  mapType Substitute.Over {} (Variable (Local.Local index)) = Variable (Local.Local index)
  mapType (Substitute.Over category) (Variable (Local.Shift index)) =
    shift $ Substitute.mapType category (Variable index)
  mapType category typex = case typex of
    Constructor index -> Constructor (Shift.map (Substitute.general category) index)
    Call function argument -> Call (Substitute.mapType category function) (Substitute.mapType category argument)
    Function parameter result ->
      Function (Substitute.mapType category parameter) (Substitute.mapType category result)
    Type universe -> Type (Substitute.mapType category universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

instance Core.Functor TypeF where
  map f = \case
    Logical logical -> Logical (f logical)
    Variable index -> Variable index
    Constructor index -> Constructor index
    Call function argument -> Call (Core.map f function) (Core.map f argument)
    Function parameter result -> Function (Core.map f parameter) (Core.map f result)
    Type universe -> Type (Core.map f universe)
    Constraint -> Constraint
    Small -> Small
    Large -> Large
    Universe -> Universe
    Levity -> Levity

infixl 9 #

(#) :: TypeF logical scope -> TypeF logical scope -> TypeF logical scope
(#) = Call

infixr 0 -#>

(-#>) :: TypeF logical scope -> TypeF logical scope -> TypeF logical scope
(-#>) = Function

variable :: Index scope -> TypeF logical scope
variable = Variable

constructor :: Type2.Index scope -> TypeF logical scope
constructor = Constructor

arrow :: TypeF logical scope
arrow = Constructor Type2.Arrow

listing :: TypeF logical scope
listing = Constructor Type2.List

list :: TypeF logical scope -> TypeF logical scope
list = Call (Constructor Type2.List)

tupling :: Int -> TypeF logical scope
tupling size = Constructor (Type2.Tuple size)

bool :: TypeF logical scope
bool = Constructor Type2.Bool

char :: TypeF logical scope
char = Constructor Type2.Char

typex :: TypeF logical scope
typex = Type Small

kind :: TypeF logical scope
kind = Type Large

typeWith :: TypeF logical scope -> TypeF logical scope
typeWith = Type

small :: TypeF logical scope
small = Small

large :: TypeF logical scope
large = Large

universe :: TypeF logical scope
universe = Universe

constraint :: TypeF logical scope
constraint = Constraint

levity :: TypeF logical scope
levity = Levity

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
