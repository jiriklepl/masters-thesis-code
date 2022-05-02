
#!/bin/sh

mkdir -p examples/out

for file in $(find examples -name "*.chmmm"); do
    cabal run -v0 Compiler < ${file} > examples/out/$(basename ${file})
done
