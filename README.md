# MetaPragma

Tired of including a long list of `LANGUAGE` pragmas at the top of
your Haskell source files? This is a preprocessor that expands
shorthand for collections of common pragmas. The only currently
defined meta-pragma is `Haskell2015` as a guess to what people might
expect to work out of the box today. It includes

- ConstraintKinds
- DataKinds
- FlexibleContexts
- FlexibleInstances
- GADTs
- LambdaCase
- MultiParamTypeClasses
- NoImplicitPrelude
- TypeFamilies
- ScopedTypeVariables

You use this preprocessor by installing it somewhere (I build it in a
sandbox, and `cabal install --symlink-bindir ~/.cabal/bin`), then, in
the source file you want to use it, you write,

```haskell
{-# META Haskell2015 #-}
{-# OPTIONS_GHC -F -pgmF metapragma #-}
```

You can include other `LANGUAGE` pragmas on other lines at the top of
the file.

## Why?

To reduce boilerplate. This saves a small amount of time, and reduces
the vagaries of how people manage long lists of pragmas.

You *can* list language extension pragmas in your `.cabal` file, but
there is a preference among some for including this information in the
source file to reduce reader surprise, and play better with
stand-alone file reading (e.g. as a blog post). While, today, nobody
knows what `Haskell2015` as used here implies, the intent is to offer
a straw man that folks can rally around or beat to the ground in order
to nail down a set of commonly used meta-pragmas (or maybe just one a
year).
