# The Hazy Haskell Compiler

Hazy is a Haskell compiler where I plan to experiment on making Haskell more
performant.

As of now the project is in a very early stages. The compiler currently unable
to bootstrap itself, the standard library isn't implemented yet, some major
Haskell features are missing, only a test Javascript backend is implemented.

# Quick Start

Ensure that GHC 9.12 and Cabal are downloaded.
Then run the following:

```sh
# Copy the runtime
cp -R runtime output

# Build the standard library
cabal run -w ghc-9.12 hazy -- -I library/runtime library/base -o output

# Build the hello world example
cabal run -w ghc-9.12 hazy -- -I library/runtime -I library/base test/run/hello/source -o output

# Optionally, Format the generated Javascript
prettier -w output

# Run hello world
node output/index.mjs
```

# TODOs

- Use `UnorderedRecords` all throughout compiler
- Add top level documentation to all modules

# Missing

- Let Pattern Binds
- Integer Literal Patterns
- Floating Point Literals
- Constrained Type Defaulting
- List Comprehensions
- Do notation
- Lambda Case
- Record Updates
- Right Sections
- Type Annotation Expressions
- Deriving
- GADTs
- Polymorphic Components
- Strict Functions

# Milestones Checklist

- [ ] Standard Library
- [ ] Bootstrapping

# Extensions
Hazy implements original extensions alongside some GHC extensions.
Note that you can only toggle the only extensions that aren't fully backwards
compatible.

## Custom Extensions

### Constructor Fields
* Pragma: `ConstructorFields`
* Toggleable: True

This is Hazy's take on `DisambiguateRecordFields`. This extension causes
constructor fields to be associated to the constructor and thus may be accessed
without needing to import said fields.

For example, this would now be valid:
```haskell
module A (Point (Point)) where

data Point = Point { x, y :: Int}
```
```haskell
module B where
import A (Point (Point))

construct = Point { x = 1, y = 2 }
```

This extension applies per constructor, so even modules that have this extension
disabled are still affected.

Additionaly, the `CONSTRUCTORFIELDS` pragma can enable this per constructor.

```haskell
data Multiple = Single Int | Many { x, y :: Int }

{-# ConstructorFields Many #-}
```

### Unordered Fields
* Pragma: `UnorderedRecords`
* Toggleable: True

This causes record declaration is this module to be unordered. This means
that they cannot be accessed with order dependent pattern matching.

Consider this example:
```haskell
data Point = Point { x, y :: Double }

dot (Point x y) = x + y -- #1
dot' Point {x, y} = x + y -- #2
```
Here, #1 is order dependent so it would be rejected while #2 is order independent
so it would be accepted.

This extension applies per constructor, so even modules that have this extension
disabled are still affected.

Additionaly, the `UNORDEREDRECORDS` pragma can enable this per constructor.

For example:
```haskell
data Multiple = Single Int | Many { x, y :: Int }

{-# UnorderedRecords Many #-}
```

### Stable Imports
* Pragma: `StableImports`
* Toggleable: True

This extension requires this property to hold:

    Your imported modules may freely add new symbols without causing overlapping symbols.

This boils down to requiring all imports either use import lists or non
overlapping qualified imports.

Consider this example:
```haskell
import A                -- #1
import B (x, Y(..))     -- #2
import qualified C      -- #3
import qualified D      -- #4
import qualified E as D -- #5
```

Import #1 is rejected because it imports everything. Import #4, #5 are rejected
because they overlap with import lists. Import #2 is okay because it uses an
import list. Import #3 is okay because it's the only qualified module in it's
namespace.

Note that exporting new constructors is considered an API break, so importing
all constructors of a type is still allowed.

This can be disabled with `{-# LANGUAGE NoStableImports #-}` or
`{-# LANGUAGE Haskell2010 #-}`.

### Of Guard Blocks
* Pragma: `OfGuardBlocks`
* Toggleable: False

Guard blocks for functions and multiway if blocks may use the `of` keyword
instead of `|`. This simply treats the block as newline intended statement.

Consider this example:
```haskell
addFail maybe1 maybe2
  | Just value1 <- maybe1
    Just value2 <- maybe2
  = value1 + value2 
  | otherwise = 0
```

This may be written like:
```haskell
addFail maybe1 maybe2
  of
    Just value1 <- maybe1
    Just value2 <- maybe2
    value1 + value2
  of
    0
```

### Extended Local Declarations
* Pragma: `ExtendedLocalDeclarations`
* Toggleable: False

Local `let` and `where` can include import, data and class declarations.

For example this code is now possible:
```haskell
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy by = map runBy . sort . map By where
  import Data.List (sort)
  newtype By = By { runBy :: a }
  instance Eq By where
    a == b
      | EQ <- compare a b = True
      | otherwise = False
  instance Ord By where
    compare (By a) (By b) = by a b
```

## GHC Extensions
### Scoped Type Variables
* Pragma: `ScopedTypeVariables`
* Toggleable: False

Hazy only implements the scoped type variables with type signaturues.
It does not implement pattern type signatures.

### Polymorphic Components
* Pragma: `PolymorphicComponents`

This is a subset of GHC's `RankNTypes`.

Planned.

### GADTs
Pragma: `GADTs`

Planned.

### Implicit Prelude
* Pragma `ImplicitPrelude`
* Toggleable: True

The implicit prelude can be turn off like in GHC. This has the same effect of
as `import Prelude()`.

### Lambda Case
* Pragma: `LambdaCase`
* Toggleable: False

Lambda case syntax is supported.

### MultiwayIf
* Pragma: `MultiwayIf`
* Toggleable: False

Multiway if syntax is supported.

### Empty Case
* Pragma: `EmptyCase`
* Toggleable: False

Empty case syntax is supported.

### Data Kinds
* Pragma: `DataKinds`
* Toggleable: False

Data kinds are supported. However, The scope of constructors does not pollude
the scope of types. This means that type level data constructors must be ticked.

If you want unticked operators, it's recommended you use type synonyms.
```haskell
data Lifted = A | B

type A = 'A
type B = 'B
```

Additionally, types have two universe levels: `type Small` and `type Large`.
Large types cannot be abstracted over or used in data types.

Consider this example:
```haskell
type BoolList = '[ 'True, 'False ] -- #1
type TypeList = '[ Int, Char ] -- #2
```

Here, #1 is okay because it's a just a lifted list of booleans.
However, #2 is a illegal because lists expect their elements to be `Type`s and
`Type` is not a `Type`.


### TypeOperators
* Pragma: `TypeOperators`
* Toggleable: False

Proper type operators are not implemented. Instead, only ticked infix constructors
are supported for completeness for `DataKinds`.

# Deviations:
## Bugs
These are deviations that will are planned to get fixed at some point.

### Newtypes are plain data
Newtype declarations are treated as normal data declarations.

### No implicit generalization
All polymorphic types must be given an annotation.

Basically, this means that all type level definitions must be given a type annotation and
that `MonoLocalBinds` is enabled.

### Constraints must have unique typeclass variable pairs
Constraints must not have overlapping typeclass / rigid variable pairs.
For example, something like this is not allowed:
```haskell
exotic :: (Eq (f Int), Eq (f Char)) => f Int -> f Int -> f Char -> f Char -> Bool
exotic a b c d = a == b && c == d
```

### Strict constructor fields are ignored
The fields of a constructor are all lazy.

### Class methods are always public
If you have access to a typeclass, then you are able to define instances for
it's methods regards on whether or not the method is exported.

For example, this is legal:
```haskell
module Hidden ( Hidden ) where

class Hidden a where
 private :: a
```
```haskell
module Usage where
import Hidden (Hidden)

data Usage = Usage

instance Hidden Usage where
  private = Usage
```

### Class default methods are ignored

The default methods in a type class are ignored. This effectively means that
all methods must be defined in an instance.

## Intentional
These are deviations that are unlikely to be fixed in the the near future.

### No Negation Operator
Hazy does not implement a negation operator. However negative integer literals
are supported. The are parsed when there is no space between the integer and the
minus sign.

This example is rejected:
```haskell
negate x = -x
```
This example get treated as sections;
```haskell
section x = (- x)
section' = (- 10)
```
This example is a normal integer literal:
```haskell
literal = (-10)
```

### Hiding imports for type does not hide constructors
Haskell 2010 specifics that `hiding` declarations must also hide constructors.
Hazy instead only hides the type constructor as one would expect.

For example:
```haskell
import Prelude hiding ( Just )
```

Will hide the `Just` constructor in Haskell, but this wouldn't hide anything in Hazy.
Instead, the syntax for hiding constructor mirrors that of import constructors.

```haskell
import Prelude hiding (Maybe (Just))
```

### No orphan instances
Hazy does not support orphan instances. All instance declarations must have
either the class or the data instance in the same module.

### Postfix Operators
Left sections are treated as function application and are not eta expanded.
This follows GHC's `PostfixOperators` extension.

# Copyright
Copyright © 2026 Freddy "Superstar64" Cubas

The compiler and all other components is licensed under GPL3 only. See `LICENSE`
for more information. The standard library and runtime (everything under the
`library` and `runtime` subdirectories) are licensed under the Boost License.