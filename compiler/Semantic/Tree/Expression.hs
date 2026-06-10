{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Expression where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Instanciation as Simple (Instanciation (..))
import qualified Core.Tree.SchemeOver as SchemeOver
import qualified Core.Tree.SchemeOver as Simple (SchemeOver)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import Semantic.Layout (Normal)
import qualified Semantic.Locality as Locality
import Semantic.Scope as Null (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (shift), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve, Unsupported)
import Semantic.Tree.Alternative (Alternative (..))
import Semantic.Tree.CallHead (CallHead)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import {-# SOURCE #-} Semantic.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Semantic.Tree.Declarations as Declarations
import Semantic.Tree.ExpressionField (Field)
import Semantic.Tree.Lambda (Lambda (..))
import Semantic.Tree.Pattern (Pattern)
import Semantic.Tree.RightHandSide (RightHandSide)
import Semantic.Tree.Scheme (Scheme)
import Semantic.Tree.Select (Select (..))
import Semantic.Tree.Statements (Statements)
import qualified Semantic.Tree.Statements as Statements (Comprehension, Do)
import Syntax.Position (Position)
import Prelude hiding (Bool (False, True), Either (Left, Right))

data Expression layout stage scope
  = CallHead
      { callHead :: !(CallHead stage scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Inferred Simple.Evidence stage scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Inferred Simple.Evidence stage scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !Text
      }
  | Tuple
      { startPosition :: !Position,
        elements :: !(Strict.Vector2 (Expression layout stage scope))
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector (Expression layout stage scope))
      }
  | Comprehension
      { startPosition :: !Position,
        statements :: !(Statements Statements.Comprehension layout stage scope)
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(Inferred ConstructorInfo stage scope),
        fields :: !(Strict.Vector (Field layout stage scope))
      }
  | Update
      { base :: !(Expression layout stage scope),
        updatePosition :: !Position,
        updates :: !(Strict.Vector1 (Select layout stage scope)),
        unsupported :: !(Unsupported stage)
      }
  | Call
      { function :: !(Expression layout stage scope),
        argument :: !(Expression layout stage scope)
      }
  | Let
      { declarations :: !(Declarations Locality.Local layout stage (Scope.Declaration ':+ scope)),
        letBody :: !(Expression layout stage (Scope.Declaration ':+ scope))
      }
  | If
      { condition :: !(Expression layout stage scope),
        thenx :: !(Expression layout stage scope),
        elsex :: !(Expression layout stage scope)
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide layout stage scope))
      }
  | Case
      { startPosition :: !Position,
        scrutinee :: !(Expression layout stage scope),
        cases :: !(Strict.Vector (Alternative layout stage scope))
      }
  | Do
      { startPosition :: !Position,
        dox :: !(Statements Statements.Do layout stage scope)
      }
  | Lambda
      { startPosition :: !Position,
        parameter :: !(Pattern stage scope),
        body :: !(Lambda layout stage (Scope.Pattern ':+ scope))
      }
  | LambdaCase
      { startPosition :: !Position,
        cases :: !(Strict.Vector (Alternative layout stage scope))
      }
  | RightSection
      { operatorPosition :: !Position,
        left :: !(CallHead stage scope),
        right :: !(Expression layout stage scope)
      }
  | Annotation
      { expression :: !(Explicit layout stage scope),
        operatorPosition :: !Position,
        annotation :: !(Scheme Position stage scope),
        instanciation :: !(Inferred Simple.Instanciation stage scope)
      }
  | RunST
      { startPosition :: !Position,
        imperative :: !(Expression layout stage scope),
        unsupported :: !(Unsupported stage)
      }
  | Negate
      { startPosition :: !Position,
        evidence :: !(Inferred Simple.Evidence stage scope),
        negative :: !(Expression layout stage scope)
      }
  deriving (Show)

instance Scope.Show (Expression layout stage) where
  showsPrec = showsPrec

instance Shift (Expression layout stage) where
  shift = shiftDefault

instance Shift.Functor (Expression layout stage) where
  map category = \case
    CallHead {callHead} ->
      CallHead
        { callHead = Shift.map category callHead
        }
    Integer {startPosition, integer, evidence} ->
      Integer
        { startPosition,
          integer,
          evidence = Shift.map category evidence
        }
    Float {startPosition, float, evidence} ->
      Float
        { startPosition,
          float,
          evidence = Shift.map category evidence
        }
    Character {startPosition, character} -> Character {startPosition, character}
    String {startPosition, string} -> String {startPosition, string}
    Tuple {startPosition, elements} ->
      Tuple
        { startPosition,
          elements = fmap (Shift.map category) elements
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = fmap (Shift.map category) items
        }
    Comprehension {startPosition, statements} ->
      Comprehension
        { startPosition,
          statements = Shift.map category statements
        }
    Record {constructorPosition, constructor, constructorInfo, fields} ->
      Record
        { constructorPosition,
          constructor = Shift.map category constructor,
          constructorInfo = Shift.map category constructorInfo,
          fields = fmap (Shift.map category) fields
        }
    Update {base, updatePosition, updates, unsupported} ->
      Update
        { base = Shift.map category base,
          updatePosition,
          updates = fmap (Shift.map category) updates,
          unsupported
        }
    Call {function, argument} ->
      Call
        { function = Shift.map category function,
          argument = Shift.map category argument
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Shift.map (Shift.Over category) declarations,
          letBody = Shift.map (Shift.Over category) letBody
        }
    If {condition, thenx, elsex} ->
      If
        { condition = Shift.map category condition,
          thenx = Shift.map category thenx,
          elsex = Shift.map category elsex
        }
    MultiwayIf {branches} ->
      MultiwayIf
        { branches = fmap (Shift.map category) branches
        }
    Case {startPosition, scrutinee, cases} ->
      Case
        { startPosition,
          scrutinee = Shift.map category scrutinee,
          cases = fmap (Shift.map category) cases
        }
    Do {startPosition, dox} ->
      Do
        { startPosition,
          dox = Shift.map category dox
        }
    Lambda {startPosition, parameter, body} ->
      Lambda
        { startPosition,
          parameter = Shift.map category parameter,
          body = Shift.map (Shift.Over category) body
        }
    LambdaCase {startPosition, cases} ->
      LambdaCase
        { startPosition,
          cases = fmap (Shift.map category) cases
        }
    RightSection {operatorPosition, left, right} ->
      RightSection
        { operatorPosition,
          left = Shift.map category left,
          right = Shift.map category right
        }
    Annotation {expression, operatorPosition, annotation, instanciation} ->
      Annotation
        { expression = Shift.map category expression,
          operatorPosition,
          annotation = Shift.map category annotation,
          instanciation = Shift.map category instanciation
        }
    RunST {startPosition, imperative, unsupported} ->
      RunST
        { startPosition,
          imperative = Shift.map category imperative,
          unsupported
        }
    Negate {startPosition, evidence, negative} ->
      Negate
        { startPosition,
          evidence = Shift.map category evidence,
          negative = Shift.map category negative
        }

instance FreeTermVariables (Expression layout) where
  freeTermVariables target = \case
    CallHead {callHead} -> freeTermVariables target callHead
    Integer {} -> []
    Float {} -> []
    Character {} -> []
    String {} -> []
    Tuple {elements} -> foldMap (freeTermVariables target) elements
    List {items} -> foldMap (freeTermVariables target) items
    Comprehension {statements} -> freeTermVariables target statements
    Record {fields} -> foldMap (freeTermVariables target) fields
    Update {base, updates} ->
      freeTermVariables target base ++ foldMap (freeTermVariables target) updates
    Call {function, argument} -> freeTermVariables target function ++ freeTermVariables target argument
    Let {declarations, letBody} ->
      concat
        [ freeTermVariables (FreeVariables.Over target) declarations,
          freeTermVariables (FreeVariables.Over target) letBody
        ]
    If {condition, thenx, elsex} ->
      concat
        [ freeTermVariables target condition,
          freeTermVariables target thenx,
          freeTermVariables target elsex
        ]
    MultiwayIf {branches} -> foldMap (freeTermVariables target) branches
    Case {scrutinee, cases} ->
      concat
        [ freeTermVariables target scrutinee,
          foldMap (freeTermVariables target) cases
        ]
    Do {dox} -> freeTermVariables target dox
    Lambda {body} ->
      freeTermVariables (FreeVariables.Over target) body
    LambdaCase {cases} -> foldMap (freeTermVariables target) cases
    RightSection {left, right} ->
      concat
        [ freeTermVariables target left,
          freeTermVariables target right
        ]
    Annotation {expression} ->
      freeTermVariables target expression
    RunST {imperative} -> freeTermVariables target imperative
    Negate {negative} -> freeTermVariables target negative

instance Connect Expression where
  connect = \case
    CallHead {callHead} ->
      CallHead
        { callHead
        }
    Integer {startPosition, integer} ->
      Integer
        { startPosition,
          integer,
          evidence = Inferred
        }
    Float {startPosition, float} ->
      Float
        { startPosition,
          float,
          evidence = Inferred
        }
    Character {startPosition, character} ->
      Character
        { startPosition,
          character
        }
    String {startPosition, string} ->
      String
        { startPosition,
          string
        }
    Tuple {startPosition, elements} ->
      Tuple
        { startPosition,
          elements = connect <$> elements
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = connect <$> items
        }
    Comprehension {startPosition, statements} ->
      Comprehension
        { startPosition,
          statements = connect statements
        }
    Record {constructorPosition, constructor, fields} ->
      Record
        { constructorPosition,
          constructor,
          constructorInfo = Inferred,
          fields = connect <$> fields
        }
    Update {base, updatePosition, updates, unsupported} ->
      Update
        { base = connect base,
          updatePosition,
          updates = connect <$> updates,
          unsupported
        }
    Call {function, argument} ->
      Call
        { function = connect function,
          argument = connect argument
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Declarations.connect declarations,
          letBody = connect letBody
        }
    If {condition, thenx, elsex} ->
      If
        { condition = connect condition,
          thenx = connect thenx,
          elsex = connect elsex
        }
    MultiwayIf {branches} ->
      MultiwayIf
        { branches = connect <$> branches
        }
    Case {startPosition, scrutinee, cases} ->
      Case
        { startPosition,
          scrutinee = connect scrutinee,
          cases = connect <$> cases
        }
    Do {startPosition, dox} ->
      Do
        { startPosition,
          dox = connect dox
        }
    Lambda {startPosition, parameter, body} ->
      Lambda
        { startPosition,
          parameter,
          body = connect body
        }
    LambdaCase {startPosition, cases} ->
      LambdaCase
        { startPosition,
          cases = connect <$> cases
        }
    RightSection {operatorPosition, left, right} ->
      RightSection
        { operatorPosition,
          left,
          right = connect right
        }
    Annotation {expression, operatorPosition, annotation} ->
      Annotation
        { expression = connect expression,
          operatorPosition,
          annotation,
          instanciation = Inferred
        }
    RunST {startPosition, imperative, unsupported} ->
      RunST
        { startPosition,
          imperative = connect imperative,
          unsupported
        }
    Negate {startPosition, negative} ->
      Negate
        { startPosition,
          evidence = Inferred,
          negative = connect negative
        }
  seperate = \case
    CallHead {callHead} ->
      CallHead
        { callHead
        }
    Integer {startPosition, integer, evidence} -> Integer {startPosition, integer, evidence}
    Float {startPosition, float, evidence} -> Float {startPosition, float, evidence}
    Character {startPosition, character} -> Character {startPosition, character}
    String {startPosition, string} -> String {startPosition, string}
    Tuple {startPosition, elements} ->
      Tuple
        { startPosition,
          elements = seperate <$> elements
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = seperate <$> items
        }
    Comprehension {startPosition, statements} ->
      Comprehension
        { startPosition,
          statements = seperate statements
        }
    Record {constructorPosition, constructor, constructorInfo, fields} ->
      Record
        { constructorPosition,
          constructor,
          constructorInfo,
          fields = seperate <$> fields
        }
    Call {function, argument} ->
      Call
        { function = seperate function,
          argument = seperate argument
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Declarations.seperate declarations,
          letBody = seperate letBody
        }
    If {condition, thenx, elsex} ->
      If
        { condition = seperate condition,
          thenx = seperate thenx,
          elsex = seperate elsex
        }
    MultiwayIf {branches} ->
      MultiwayIf
        { branches = seperate <$> branches
        }
    Case {startPosition, scrutinee, cases} ->
      Case
        { startPosition,
          scrutinee = seperate scrutinee,
          cases = seperate <$> cases
        }
    Do {startPosition, dox} ->
      Do
        { startPosition,
          dox = seperate dox
        }
    Lambda {startPosition, parameter, body} ->
      Lambda
        { startPosition,
          parameter,
          body = seperate body
        }
    LambdaCase {startPosition, cases} ->
      LambdaCase
        { startPosition,
          cases = seperate <$> cases
        }
    RightSection {operatorPosition, left, right} ->
      RightSection
        { operatorPosition,
          left,
          right = seperate right
        }
    Annotation {expression, operatorPosition, annotation, instanciation} ->
      Annotation
        { expression = seperate expression,
          operatorPosition,
          annotation,
          instanciation
        }
    Negate {startPosition, evidence, negative} ->
      Negate
        { startPosition,
          evidence,
          negative = seperate negative
        }

data Explicit layout stage scope where
  Explicit :: !(Expression layout Resolve (Scope.Local ':+ scope)) -> Explicit layout Resolve scope
  Known :: !(Simple.SchemeOver (Expression layout Check) scope) -> Explicit layout Check scope

instance Show (Explicit layout stage scope) where
  showsPrec d = \case
    Explicit expression -> showParen (d > 10) $ showString "Explicit " . showsPrec 11 expression
    Known expression -> showParen (d > 10) $ showString "Known " . showsPrec 11 expression

instance Shift (Explicit layout stage) where
  shift = shiftDefault

instance Shift.Functor (Explicit layout stage) where
  map category = \case
    Explicit expression -> Explicit (Shift.map (Shift.Over category) expression)
    Known expression -> Known (Shift.map category expression)

instance FreeTermVariables (Explicit layout) where
  freeTermVariables target (Explicit expression) = freeTermVariables (FreeVariables.Over target) expression

instance Connect Explicit where
  connect (Explicit expression) = Explicit (connect expression)
  seperate (Known expression) = Known (SchemeOver.map (SchemeOver.Map seperate) expression)

callHead_ :: CallHead Resolve scope -> Expression Normal Resolve scope
callHead_ callHead = CallHead {callHead}
