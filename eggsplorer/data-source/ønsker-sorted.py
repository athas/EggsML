#!/usr/bin/env python3
#
# Requests sorted in descending order by the number of times a product has
# been requested.

import subprocess
import itertools


def run():
    pairs = [(int(stamp), title) for stamp, title in
             (line.split(' ', 1) for line in
              subprocess.check_output(['./ønsker.sh']).decode('utf-8').strip().split('\n'))]
    pairs.sort(key=lambda k: (k[1], k[0]))
    reqs = [(name, [stamp for stamp, title in sub_pairs])
            for name, sub_pairs in itertools.groupby(pairs, key=lambda k: k[1])]
    reqs.sort(key=lambda k: len(k[1]))
    reqs = list(filter(lambda k: len(k[1]) > 1, reqs))[::-1]
    for title, stamps in reqs:
        print(title)
        print(stamps)
        print()


if __name__ == '__main__':
    run()
