#!/bin/sh
#
# Bare kør den uden nogen argumenter.

curl -s http://kongehuset.dk/monarkiet-i-danmark/nytarstaler/hendes-majestat-dronningens-nytarstaler \
    | grep Nytårstalen | grep '<a' | grep field-item | grep -Eo 'http[^"]+' | while read url; do
    echo Getting $url > /dev/stderr
    year=$(echo $url | grep -oE '[0-9]+')
    echo Year: $year > /dev/stderr
    curl -s $url | grep -E '<p>|<br' | grep -Ev '<a|\||COPYRIGHT' \
        | sed -r -e 's/<[^>]+>//g' -e 's/^\s+//' -e 's/&[^;]+;//g' \
        | grep -Ev '^\s*$' | grep -E '\w' > "$(dirname "$0")/$year.txt"
    sleep 1
done
