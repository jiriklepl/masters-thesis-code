#!/bin/sh

which cabal 2> /dev/null 1> /dev/null

[ $? -ne 0 ] && echo "Please install cabal" && exit 1

USE_LLC=$(which llc-12 || which llc12 || which llc-13 || which llc13 || which llc-14 || llc14 || which llc) 2> /dev/null

[ "$USE_LLC" = "" ] && echo "Please install llc (preferably version 12)" && exit 1

run_tests() {

find examples -maxdepth 1 -mindepth 1 -name "*.chmmm" | {
    TOTAL=0
    SUCCESS=0
    FAILURE=0
    FAILED=""

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

        if cabal run -v0 Compiler -- -o "$out" "$file" && "$USE_LLC" -o /dev/null "$out"; then
            RESULT_C=1
        else
            RESULT_C=0
        fi

        SUCCESS=$(( SUCCESS + FAILURE_C * RESULT_C + (1 - FAILURE_C) * (1 - RESULT_C) ))
        OLD_FAILURE="$FAILURE"
        FAILURE=$(( FAILURE + (1 - FAILURE_C) * RESULT_C + (FAILURE_C) * (1 - RESULT_C) ))

        if [ "$OLD_FAILURE" -lt "$FAILURE" ]; then
            FAILED="$file, $FAILED"
        fi
    done

    echo ""
    echo ""
    echo ""

    echo "SUCCESS: $SUCCESS (including expected exits on wrong inputs)"
    echo "FAILURE: $FAILURE (including unexpected exits)"
    echo "TOTAL: $TOTAL"

    if [ "$SUCCESS" -lt "$TOTAL" ]; then
        echo "FAILED:"
        echo "$FAILED"

        exit 2
    fi
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
