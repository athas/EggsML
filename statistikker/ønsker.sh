#!/bin/sh

oensker() {
    # Format per linje: <unix timestamp> <mellemrum> <ønske>
    git log --pretty=format:'%an:%at:%s' \
        | grep -i '^concieggs' \
        | grep -E ':[^:]+:[^:]+: ønsk' \
        | sed -r 's/[^:]+:([^:]+):[^:]+: ønsk ([0-9]+ af )?(.+)/\1 \3/'
}

oensker
