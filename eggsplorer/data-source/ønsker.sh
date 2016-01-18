#!/bin/sh
#
# Get all food requests ("ønsker") from the git log.

oensker() {
    # Output format per line: <unix timestamp> <space> <food request>
    git log --pretty=format:'%an:%at:%s' \
        | grep -i '^concieggs' \
        | grep -E ':[^:]+:[^:]+: ønsk' \
        | sed -r 's/[^:]+:([^:]+):[^:]+: ønsk ([0-9]+ af )?(.+)/\1 \3/'
}

oensker
