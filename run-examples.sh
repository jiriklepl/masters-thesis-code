#!/bin/sh


run_tests() {

find examples -maxdepth 1 -mindepth 1 -name "*.chmmm" | {
    TOTAL=0
    SUCCESS=0
    FAILURE=0

    while read -r file; do
        TOTAL=$((TOTAL+1))
        out="examples/out/$(basename "$file")"
        FAILURE_C=1
        case "$file" in
            *-bad-*)
            FAILURE_C=0
            ;;
            *)
            ;;
        esac

        echo "doing $file" > /dev/stderr

        if cabal run -v0 Compiler -- -o "$out" "$file" && llc-12 -o /dev/null "$out"; then
            RESULT_C=1
        else
            RESULT_C=0
        fi
        SUCCESS=$(( SUCCESS + FAILURE_C * RESULT_C + (1 - FAILURE_C) * (1 - RESULT_C) ))
        FAILURE=$(( FAILURE + (1 - FAILURE_C) * RESULT_C + (FAILURE_C) * (1 - RESULT_C) ))
    done

    echo "SUCCESS: $SUCCESS"
    echo "FAILURE: $FAILURE"
    echo "TOTAL: $TOTAL"

    [ "$SUCCESS" -lt "$TOTAL" ] && exit 2
}

}

case $1 in
    run)
        run_tests
    ;;
    safe)
        mkdir -p examples/out

        cabal build Compiler || exit 1

        systemd-run --scope -p MemoryMax=500M "$0" run
    ;;
    *)
    mkdir -p examples/out

    cabal build Compiler || exit 1

    run_tests
    ;;
esac
