# LDST - Label dependent session types

This repository contains an implementation of a frontend (parser and
type checker) and a backend for LDST (formerly ldgv).

There also is a small [article](article.md) documenting the backend.

## Requirements

The [nix packet manager](https://nixos.org/nix/), preferably with a binary cache set up
like described [here](https://github.com/obsidiansystems/obelisk/blob/master/README.md).

Cabal is needed to build.

## Build

In the toplevel directory:

`nix-build -A ghcjs.ldgv`

to build the nix package.

To build incrementally use the nix shell for ghcjs (website) `nix-shell -A shells.ghcjs`,
configure cabal to use ghcjs `cabal configure --ghcjs` and build with `cabal build`. 

## Test the Parser

Run

`nix-shells -A shells.ghc`

and execute

`cabal test`

to let [hspec](https://hspec.github.io/) discover and run all tests.

Be sure to `cabal configure --ghc` beforehand.

## Usage

After building with ghcjs you will find an `index.html` in `results/bin/ldgv.exe`.

The input syntax is explained in file `syntax.txt` (also on the resulting website).
There are examples in the `examples` directory. Source files end in `.ldgv`. 
