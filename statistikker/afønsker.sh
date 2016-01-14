#!/bin/sh

afoensker() {
    # Format per linje: <unix timestamp> <mellemrum> <ønske>
    git log --pretty=format:'%an:%at:%s' \
        | grep -i '^concieggs' \
        | grep -E ':[^:]+:[^:]+: afønsk' \
        | sed -r 's/[^:]+:([^:]+):[^:]+: afønsk ([0-9]+ af )?(.+)/\1 \3/'
}

afoensker
