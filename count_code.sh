#!/bin/sh

count_imports() {
    find src -name "*.hs" -type f -print0 \
        | xargs -0 awk 'BEGIN{n=0}BEGINFILE{module=0}/^ *$|^--/{next}{n+=1}/^module/{module=1;next}/^import|^ /{next}module==1{n-=1;nextfile}END{print n}'
}
count_all() {
    find src -name "*.hs" -type f -print0 \
        | xargs -0 cloc --sum-one --csv \
        | tail -n1 \
        | awk -F, '{print $NF}'
}

imports="$(count_imports)"
all="$(count_all)"

echo $((all - imports))
