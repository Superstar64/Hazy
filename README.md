# The Hazy Haskell Compiler

Hazy is a Haskell compiler where I plan to experiment on making Haskell more
performant.

As of now the project is in a very early stages. The compiler currently unable
to bootstrap itself, the standard library isn't implemented yet, some major
Haskell features are missing, only a test Javascript backend is implemented.

# TODOs

- Use `UnorderedRecords` all throughout compiler
- Add top level documentation to all modules

# Missing

- Floating Point Literals
- Constrained Type Defaulting
- List Comprehensions
- Record Updates
- Deriving
- GADTs
- Polymorphic Components
- Strict Functions
- Some Builtin Instances

# Milestones Checklist

- [ ] Standard Library
- [ ] Bootstrapping

# Quickstart

Ensure that GHC 9.12 and Cabal are downloaded.

```sh
# Run the build script
tools/build.sh .dist

# Add the compiler to the path for testing
export PATH=.dist/bin:$PATH

# Compile hello world
hazy test/run/hello/source -o .hello

# Run hello world
node .hello/index.mjs
```

# Packages

Hazy has a bare minimum package system at the moment. A package is a folder with
three subfiles:
* `$PACKAGE/package`: This contains the package metadata. As of now, it only
   includes the default extensions and the list of Haskell files in the package.
* `$PACKAGE/header`: This contains the compiler generated header files that are
   not looked at for code generation. As of now, these files are just copies of
   their original source files.
* `$PACKAGE/artifacts`: This contains the generated Javascript that Hazy needs
   to copy when generating a final executable.

To generate a package, just compile a set of modules and pass `--pack`.
For example:
```sh
hazy Module.hs modules/ -o mypackage --pack
```

To load a package, pass `--package $PATH`. For example:
```
hazy --package mypackage -o program
```
This causes of all of the packages modules to be added to the module list and,
when generating a full executable, causes the package's Javascript to be copied
to the final program.

By default hazy will load `runtime` and `base` from it's `packages` folder.
Other packages aren't special cased and any path can be loaded with `--package`.

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

### Extended Function Bindings 
* Pragma: `ExtendedFunctionBindings`
* Toggleable: False

Function bindings are not requried to be contiguous and they may have different
number of clauses.

For example, this is legal:
```haskell
notJust (Just bool) = Just (not bool)
id x = x
notJust = id
```

Here, both `notJust` are combined into a single function and the second case
of `notJust` is eta expanded.

### Extended Import Declarations
* Pragma: `ExtendedImportDeclarations`
* Toggleable: False

Import declarations may appear anywhere inside a module.

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

### NamedFieldPuns
* Pragma: `NamedFieldPuns`

Named field pun syntax is supported.

# Deviations:
## Bugs
These are deviations that will are planned to get fixed at some point.

### No binding groups
Hazy does not generalize bindings groups. Unannotated declarations that form a
cycle are errors.

For example, this would be rejected:
```haskell
f a b = g a b
g a b = f a b
```
However, this would be okay:
```haskell
f a b = a b
g a b = f a b
```

### Constraints must have unique typeclass variable pairs
Constraints must not have overlapping typeclass / rigid variable pairs.
For example, something like this is not allowed:
```haskell
exotic :: (Eq (f Int), Eq (f Char)) => f Int -> f Int -> f Char -> f Char -> Bool
exotic a b c d = a == b && c == d
```

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

## Planned Toggles
These are devitations that will eventually toggleable with a language pragma.

### Universal monomorphic restriction
Hazy applies the monomorphism restriction to all declarations. This means that
only type variables without constraints are generalized.

Consider these two examples:
```haskell
f a = a      -- #1
f' a = a + 1 -- #2
```
Here, `#1` is polymorphic over an unconstrainted type variable so it gets
properly generalized to `f :: a -> a`. However, `#2` is rejected because it
would have the type `f' :: Num a => a -> a`.

### MonoLocalBinds Only
All local bindings are monomorphic. This is nearly equivalent to GHC's
`MonoLocalBinds` extension.

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

### Postfix Operators
Left sections are treated as function application and are not eta expanded.
This follows GHC's `PostfixOperators` extension.

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

## Intentional
These are deviations that are unlikely to be fixed in the the near future.

### No orphan instances
Hazy does not support orphan instances. All instance declarations must have
either the class or the data instance in the same module.


# Copyright
Copyright © 2026 Freddy Angel Cubas "superstar64"

The compiler and other components are licensed under GPL3 only unless otherwise
specified. See `LICENSE` for more information. The standard library, the
runtime, and extra utilities (everything under the `library`, `runtime` and
`utility` subdirectories) are licensed under the Boost License.

Any code generated by the compiler follows the license of the input source code.
This includes snippets inserted by compiler.