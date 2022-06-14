#!/bin/sh

mkdir -p examples/out

cabal build Compiler || exit 1

for file in $(find examples -maxdepth 1 -mindepth 1 -name "*.chmmm"); do
    cabal run -v0 Compiler < ${file} > examples/out/$(basename ${file})
done
