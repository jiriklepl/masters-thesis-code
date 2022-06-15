#!/bin/sh

mkdir -p examples/out

cabal build Compiler || exit 1

find examples -maxdepth 1 -mindepth 1 -name "*.chmmm" | {
    TOTAL=0
    SUCCESS=0
    FAILURE=0

    while read -r file; do
        TOTAL=$((TOTAL+1))
        out="examples/out/$(basename "$file")"
        if cabal run -v0 Compiler < "$file" > "$out"; then
            SUCCESS=$((SUCCESS+1))
        else
            FAILURE=$((FAILURE+1))
        fi
    done

    echo "SUCCESS: $SUCCESS"
    echo "FAILURE: $FAILURE"
    echo "TOTAL: $TOTAL"

    [ "$SUCCESS" -lt "$TOTAL" ] && exit 2
}