#!/bin/sh

tailAfter() {
    for i in $(seq 1 $1); do
        read ignore_this
    done
    cat
}

cat FVN \
    | tailAfter 3 \
    | egrep -v '\.\.\.' \
    | cut -d ' ' -f 3- \
    | sed -r 's/^[†$M]+ //' \
    | egrep -v '^=' \
    | egrep -v '^ *°' \
    | egrep -v '[\(\)\{\}\<\>/§†\$[]' \
    | egrep '.{30,}' \
    | sed -r 's/^ +//' \
          > gode-sætninger
