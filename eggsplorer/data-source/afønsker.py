#!/usr/bin/env python3
#
# Get all food un-requests ("afønsker") from the git log.

import re
import subprocess
import sys

# Output format per line: <unix timestamp> <space> <food un-request>
def run():
    command = ['git', 'log', '-p', '--pretty=format:%n%%%an:%at:%s']
    commits = subprocess.check_output(command).strip().split(b'\n%')
    commits = filter(lambda c: (c.startswith(b'concieggs')
                                or c.startswith(b'Concieggs Nippenauer')),
                     commits)
    commit_line_re = rb'[^:]+:[^:]+:[^:]+: (af\xc3\xb8nsk|[^:]+: Fjern alle indk\xc3\xb8bs\xc3\xb8nsker)'
    commits = filter(lambda c: re.match(commit_line_re, c), commits)
    for com in commits:
        com = com.decode('utf-8')
        line, diff_intro, diff = com.split('\n', 2)
        assert diff_intro.startswith('diff')
        timestamp = line.split(':')[1]
        diff = diff.split('@@', 1)[1]
        afønsker = filter(lambda line: line.startswith('-'), diff.split('\n'))
        afønsker = map(lambda line: line[1:].lower(), afønsker)
        afønsker = list(set(afønsker))
        for a in afønsker:
            print(timestamp, a)

if __name__ == '__main__':
    run()
