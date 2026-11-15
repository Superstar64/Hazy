module Core.Tree.Type where

import Core.Substitute (Category (..))
import qualified Core.Substitute as Substitute
import qualified Core.Type.Show as Core (Show (..))
import qualified Core.Type.Substitute as Type.Substitute
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Local (Index)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Singleton (Singleton))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
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
  map category = \case
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
  map = Type.Substitute.mapDefault

instance Type.Substitute.Functor TypeF where
  map category = \case
    Logical logical -> Logical (Type.Substitute.mapType category logical)
    Variable index
      | Strict.Just typex <- Substitute.lookupLocal (Type.Substitute.category category) index -> typex
      | otherwise -> Variable $ Shift.map (Type.Substitute.general category) index
    Constructor index -> Constructor (Shift.map (Type.Substitute.general category) index)
    Call function argument ->
      Call
        (Type.Substitute.map category function)
        (Type.Substitute.map category argument)
    Function parameter result ->
      Function (Type.Substitute.map category parameter) (Type.Substitute.map category result)
    Type universe -> Type (Type.Substitute.map category universe)
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

strict, lazy :: TypeF logical scope
strict = Constructor Type2.Strict
lazy = Constructor Type2.Lazy

simplify :: Solved.Type position Check scope -> Type scope
simplify typex = simplifyWith typex []

simplifyWith :: Solved.Type position Check scope -> [Type scope] -> Type scope
simplifyWith Solved.Constructor {constructor, synonym} arguments = case synonym of
  Solved.Synonym length synonym -> foldl Call (Type.Substitute.map category synonym) kept
    where
      erased = take length arguments
      kept = drop length arguments
      targets = Substitute.Types $ Strict.Vector.fromList erased
      category = Type.Substitute.lift $ SubstituteLocal Shift.Id targets Singleton
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
