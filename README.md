Haskell Code Guidelines
=======================

This document describes the preferred code style for Haskell at
[Hornetsecurity](https://hornetsecurity.com). It is based on Simon
Meier's
[Style Guide for Elevence](git@github.com:meiersi/HaskellerZ.git).

If your're interested in an exciting career with Haskell have a look at our career page:

https://www.hornetsecurity.com/de/karriere



## Guiding Principles

* These rules are not set in stone.  If you have good reason to break
  a rule, break it.

* Optimize for readability and ease of reasoning about code.  Code
  that is easy to read and understand offers fewer opportunities for
  bugs to hide.

* Enlist the help of the compiler and type system.  Good types make it
  harder to make mistakes.

* Prefer code layout that scales to large numbers of functions and
  modules.


## Project Layout

### Use Stack

[Stack](https://stackage.org/) is the preferred build tool.  A
template file for new projects is included in this repository.


### Warnings

Code should be compilable with `-Wall -Werror`. There should be no
warnings.  All unit tests, including `hlint`, should pass.


### Directory Layout

The toplevel project directory should always contain

- the projects current `stack.yaml`,
- the projects cabal file and `Setup.hs`,
- a `README` file, preferably in Markdown syntax,
- a `LICENSE` file, preferably plain text,
- optionally older stack configuration files that are tested to work.

The projects library source resides in the `src` directory, test code
within the `test` directory.


### Recommended Libraries

- Control
  - `mtl` (or `transformers` for simple stuff)
- Data Structures
  - `bytestring`
  - `text`
  - `containers`
  - `unordered-containers`
- Parsing
  - `attoparsec`
- Testing
  - `tasty`
  - `hunit` (via `tasty-hunit`)
  - `smallcheck` and `QuickCheck` (via `tasty-smallcheck` and `tasty-quickcheck`)
  - `hlint`
- Benchmarking
  - `criterion`


## Formatting

These formatting rules are implemented in style "Cramer" in the
[hindent](https://github.com/chrisdone/hindent) tool.

### Line Length

Maximum line length is 80 characters.  Lines may occasionally exceed
80 characters if wrapping earlier would be awkward.


### Indentation

The basic unit of indentation is 4 spaces.  Certain construct may use
half-indents of 2 spaces. Tabs are never allowed.


### Blank Lines

Use blank lines to aid readability.

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.


### Whitespace

Surround binary operators with a single space on either side.  Add no
space inside parentheses, but do add space inside brackets and braces.
Commas are followed by one space.

Don't insert a space after a lambda.


### Alignment

Align the constructors in a data type definition, fields in a
record declaration, and elements in a list.

Separators (`|` or `,`) are placed after a newline and followed by a
single space.  They line up with the opening character (`=`,
`[`, or `{`). The closing character stands on a line by its own.

Short enums and lists may be put on a single line.

Example:

```haskell
data Either a b = Left a
                | Right b

data Point = Point { x :: Int
                   , y :: Int
                   }

exceptions :: [StatusCode]
exceptions = [ InvalidStatusCode
             , MissingContentHeader
             , InternalServerError
             ]
```


### Pragmas

Put pragmas immediately before the function/constructor/field they apply to.
Example:

```haskell
{-# INLINE id #-}
id :: a -> a
id x = x
```


### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda.  Use
your judgment. Some examples:

```haskell
bar :: IO ()
bar =
    forM_ [1, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n

foo :: IO ()
foo =
    alloca 10 $ \a ->
    alloca 20 $ \b ->
    cFunction a b
```


### Export Lists

Format export lists as follows:

```haskell
module Data.Set
    ( -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

A single item export list may be written on one line:

```haskell
module Main ( main ) where
```


### Where Clause

The `where` keyword appears on a line by itself and is indented by 2
spaces to set it apart from the rest of the code. The bindings in a
`where` clause are indented an additional 2 spaces.

```haskell
foldr f i = go
  where
    go []     = i
    go (x:xs) = x `f` go xs
```


### Let-In Expressions

The bindings in a `let` or `let in` clause should be aligned and
directly follow the `let` keyword.  The `in` keyword appears on a line
by itself, to set it apart from the rest of the code, with the
expression indented by 4 spaces.

```haskell
let x = ...
    y = ...
in
    x + y
```

When the expression inside a `let in` is a `do` block, the `do`
keyword is put on the same line as the `in` keyword to avoid overly
much vertical whitespace.

```haskell
let x = ...
    y = ...
in do
    return (x + y)
```


### If-Then-Else Expression

Generally, guards and pattern matches should be preferred over
`if then else` clauses, where possible.  Short cases should usually be
put on a single line, as line length permits.

When writing non-monadic code (i.e. when not using `do`), align the
`if`, `then`, and `else` keywords:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, indent the `then` and `else` by one unit.

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```


### Case Expressions

Pattern in `case` statements are indented with 4 spaces.

```haskell
case foo x of
    False -> return ()
    True  -> do
        line <- getLine
        process line
```


### Do Blocks

The `do` keyword should be followed by a line break and the block's
statements indented by 4 spaces with respect to the previous line.

```haskell
main = do
    name <- getLine
    putStrLn $ "Hello " ++ name ++ "!"
```


## Imports

Imports should be sorted alphabetically and grouped by top-level
module-hierarchy name. Align common keywords per import group and break
explicit import lists as follows.

```haskell
import           Control.Lens        ( preview, ix, at, traverseOf, toListOf
                                     , view, use )
import qualified Control.Monad.Catch as Catch
```

Always prefer explicit import lists or `qualified` imports.  This makes the
code more robust against changes in the imported modules.

For qualified imports you should either use the full or abbreviated name of
the last name(s) in the module hierarchy. Here are a few examples.

```haskell
import qualified Control.Monad.Catch  as Catch

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Data.HashMap         as HM
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
```


## Naming

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.


### Abbreviations

Avoid unprincipled abbreviations; in particular when naming top-level
functions.


### Records and Constructors

Prefix record fields either with the full name of the type or with the
abbreviated name of the type. For example,

```haskell
data EmailAddress = EmailAddress { eaName   :: !Text
                                 , eaDomain :: !Text
                                 }
```

or

```haskell
data EmailAddress = EmailAddress { emailAddressName   :: !Text
                                 , emailAddressDomain :: !Text
                                 }
```

If you need to disambiguate constructors, then do this by post-fixing
either the full or abbreviated name of the type. For example,

```haskell
data ValidationError = ReferenceVE !Reference
                     | CharacterVE !Char
```

or

```haskell
data ValidationError = ReferenceValidationError !Reference
                     | CharacterValidationError !Char
```


### Module Names

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.


### Name Spaces

Avoid repeating a module's name in the name of the types and values it is
defining. In particular avoid abbreviating the actual interesting part of the
name in favor of repeating the module name. Modules form name spaces that
should be made use of. For example,

```haskell
-- Bad
module Foo.Bar where

data BarS = A | B
```

```haskell
-- Good
module Foo.Bar where

data State = A | B
```

## Comments

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level declaration, particularly everything exported,
and provide type signatures. Use Haddock syntax in the comments.


## Dealing with Laziness

By default, use strict data types and lazy functions.

### Data Types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
data Point = Point { pointX :: !Double  -- ^ X coordinate
                   , pointY :: !Double  -- ^ Y coordinate
                   }
```


### Function Arguments

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
sum :: [Int] -> Int
sum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```
