#!/bin/sh
#
# Bare kør den uden nogen argumenter.

find_for_dato() {
    maaned="$1"
    dag="$2"

    curl -s http://kongehuset.dk/calendar-day/ajax/2018-$maaned-$dag/all \
        | python3 -c 'import sys, json; print(list(json.load(sys.stdin)[1].values())[3])' \
        | grep fødselsdag | sed -r 's/\s*<[^>]+>\s*//g'
}

udfil="$(dirname "$0")/royale_fødselsdage"

for maaned in $(seq 1 12); do
    maaned="$(printf %02d $maaned)"
    for dag in $(seq 1 31); do
        dag="$(printf %02d $dag)"
        echo "Tjekker $maaned-$dag..." > /dev/stderr
        foesda="$(find_for_dato $maaned $dag)"
        if [ "$foesda" ]; then
            echo "$maaned-$dag $foesda"
        fi
        sleep 1
    done
done | tee "$udfil"
