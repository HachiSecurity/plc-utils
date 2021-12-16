# Utility library for Plutus Core

A small Haskell library which contains types and functions for working with Plutus Core, which may be useful across different projects. The library currently offers the following:

- A modular representation of the typed and untyped Plutus Core abstract syntax.
- Smart constructors for constructing untyped Plutus Core terms, e.g. for writing tests.
- Helper functions for parsing/serialising typed/untyped Plutus Core programs.

## Usage

Before you can use this library, add it as a dependency in your `stack.yaml`

```yaml
extra-deps:
  - git: git@github.com:HachiSecurity/plc-utils
    commit: a6fbc2eb9fbef5fd48d14bf69faab5c1743da61a
```

or `cabal.project`

```cabal
source-repository-package
  type: git
  location: git@github.com:HachiSecurity/plc-utils
  tag: a6fbc2eb9fbef5fd48d14bf69faab5c1743da61a
```

Then add it as a dependency to your `.cabal` or `package.yaml`.

To generate the documentation for this library, run e.g. `stack haddock`.

## Modular term representation

The `PlutusCore.TermF` module exports a modular representation of the Plutus Core AST that is divided into three types:

- `TermF` which represents abstract syntax that is shared between typed and untyped Plutus Core.
- `UntypedF` which represents abstract syntax that is exclusive to untyped Plutus Core.
- `TypedF` which represents abstract syntax that is exclusive to typed Plutus Core.

The abstract syntax of typed and untyped Plutus Core can then be recovered by taking the sum of these functors. In other words, `Sum (TermF ...) (UntypedF ...)` represents the abstract syntax of untyped Plutus Core. We export two type synonyms:

- `UntypedTermF` which represents the abstract syntax of untyped Plutus Core.
- `TypedTermF` which represents the abstract syntax of typed Plutus Core.

To convert from the ordinary Plutus Core AST to this module representation, we have the following two functions:

- `convertUPLC` converts an untyped Plutus Core `Term` to a representation using `UntypedTermF`.
- `convertPLC` converts a typed Plutus Core `Term` to a representation using `TypedTermF`.

The modular representation allows us to interweave additional types of nodes into either AST. For example, if we wanted to extend the abstract syntax of untyped Plutus Core with `let`-bindings, we could simply implement the following extension to the abstract syntax:

```haskell
data LetTermF name r
    = Let name r r
    deriving (Eq, Show, Functor)

type UntypedLetTermF name const ann =
    Sum (UntypedTermF name const ann) (LetTermF name)
```

As a short example of how this might be used, consider a function which attempts to turn all function applications into `let`-bindings:

```haskell
restoreLets
    :: Fix (UntypedTermF name DefaultConstant ann)
    -> Fix (UntypedLetTermF name DefaultConstant ann)
restoreLets (Fix (InL (Apply _ (Fix (InL (LamAbs _ name body))) arg))) =
    Fix $ InR $ Let name (restoreLets body) (restoreLets arg)
restoreLets (Fix term) = Fix $ InL $ fmap restoreLets term
```