# CHMMM

This repository contains a prototype compiler for my master's thesis.

It is a proof-of-concept implementation for a modified version of the C-- language showcasing type inference in the context of systems programming and inference-guided automatic resource management.

## Dependencies

- cabal (version 3.2+; preferably version 3.6.2 acquired through ghcup)
- ghc (version 8.8.4 - 8.10.7; preferably version 8.10.7 acquired through ghcup)
- llc (version 12+; preferably version 12)

## Building

```sh
git submodule init
git submodule update
cabal build --only-dependencies Compiler
cabal build Compiler

# documentation:
cabal haddock Compiler
```

## Running

For general use:

```sh
cabal run Compiler -- [options...] input_file
```

For running the examples (tests):

```sh
./run_examples.sh
```

For more information, try:

```sh
cabal run Compiler -- -h
```

For `/dev/stdout` output, use:

```sh
cabal run Compiler -- -o - input_file
```

## Modifications

