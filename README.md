# CHMMM

This repository contains the prototype compiler for my master's thesis.

It is a proof-of-concept implementation for a modified version of the C-- language showcasing type inference in the context of systems programming and inference-guided automatic resource management.

## Dependencies

- cabal (version 3.2+; preferably version 3.6.2 acquired through ghcup)
- ghc (version 8.8.4 - 9.0.2; preferably version 8.10.7 acquired through ghcup)
- llc (version 12+; preferably version 12)

## Building

```sh
git submodule init
git submodule update
cabal build --only-dependencies Compiler
cabal build Compiler

# development documentation:
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
# the examples that contain "-bad-" in their name
#   represent incorrect inputs
```

For more information:

```sh
cabal run Compiler -- -h
```

For `/dev/stdout` output, use:

```sh
cabal run Compiler -- -o - input_file
```

## Development documentation

The source code is documented with Haddock. You can build the HTML documentation by running:

```sh
cabal haddock Compiler
```

This command is quite verbose but its output ends with the location of the index file of the generated documentation (it should be somewhere in the `dist-newstyle` folder).

When reading the source files of the program, we suggest using Haskell Language Server (HLS), which parses the documentation comments and makes the documentation more easily accessible.

### Development requirements

The source files are formatted by `hindent` and checked by `hlint` (should not produce any hints). The source should compile without and warnings and the script `.\run_examples.sh` should compile all files and succesfully interpret them by `llc`.

### Project outline

The project contains many modules of varying significance documented with Haddock, here we list the main ones:

- `CMM.Pipeline`: contains the high-level logic of the compiler and wrappers for the main phases of the compiler pipeline
- `CMM.Lexer`: contains the definition of the tokenization phase
- `CMM.Parser`: contains the definition of the parsing phase
- `CMM.AST`: contains the definitions of various abstract syntactic tree (AST) nodes used as an representation of the program
- `CMM.AST.Flattener`: defines the function `flatten`, which flattens the given AST
- `CMM.AST.Blockifier`: defines the function `blockify`, which blockifies the procedures in the given AST, annotating each statement with a block annotation (that assigns the given statement to a corresponding basic block). It also produces the `BlockifierState`, which is refined by `CMM.FlowAnalysis`
- `CMM.FlowAnalysis`: defines the flow analysis for a given procedure. It is issued by `CMM.Blockifier` and refines its `BlockifierState`
- `CMM.Inference.Preprocess`: contains the definition of the function `preprocess`, which performs the inference preprocessing phase: elaborates the AST and generates the constraints that represent the type semantics of the program
- `CMM.Inference`: defines the function `reduce`, which performs the inference pipeline on the given set of constraints (facts), producing an `InferencerState` that can interpret types of the elaborated AST
    - The various data are defined in: `CMM.Inference.Type`, `CMM.Inference.TypeCompl`, `CMM.Inference.TypeKind`, `CMM.Inference.Properties`, `CMM.Inference.Fact`, `CMM.Inference.DataKind` , `CMM.Inference.Constness`
- `CMM.Monomorphize`: contains the definition of the function `monomorphize`, which monomorphizes the given program represented by elaborated AST. It uses the `InferencerState` to interpret each type
- `CMM.FillElabs`, `CMM.Mangle`: these two modules define the postprocessing of monomorphized code - filling-in concrete types in place of type variables according and name-mangling of monomorphic copies of polymorphic top-level definitions
- `CMM.Translator`: contains the definition of the function `translate`, which performs the translation phase on an elaborated and blockified AST - emission of the LLVM assembly. It uses `BlockifierState` and `InferencerState` to interpret the control flow and types, respectively
