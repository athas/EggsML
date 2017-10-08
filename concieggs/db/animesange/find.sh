#!/bin/sh

outfile="$(dirname "$0")/jpopsanglinjer.txt"

lyrics_links() {
    curl -s 'http://www.lyricsmode.com/search.php?search=japanese%20version' \
        | grep -Eo '<a href="/lyrics/./[^/"]+/[^"]+' \
        | cut -d '"' -f 2 \
        | sed 's/^/http:\/\/www.lyricsmode.com\//'
}

sentences() {
    curl -s "$1" \
        | grep -E '<br />$' \
        | sed -r 's/<[^\>]+>/\n/g' \
        | grep -E . \
        | tail -n +2 \
        | sed -r 's/\[[^]]+\] *//g' \
        | grep -E '^[^:]+$'
}

lyrics_links | while read link; do
    sentences "$link"
done | tee "$outfile"
