#!/usr/bin/env python3

import subprocess
import itertools
import json


def run():
    pairs = [(int(stamp), title.lower()) for stamp, title in
             (line.split(' ', 1) for line in
              subprocess.check_output(['./ønsker.sh']).decode('utf-8').strip().split('\n'))]
    pairs.sort(key=lambda k: (k[1], k[0]))
    reqs = [(name, [stamp for stamp, title in sub_pairs])
            for name, sub_pairs in itertools.groupby(pairs, key=lambda k: k[1])]
    reqs.sort(key=lambda k: len(k[1]))
    reqs = list(filter(lambda k: len(k[1]) > 1, reqs))[::-1]

    lmin, lmax = reqs[0][1][0], reqs[0][1][0]
    ds = []
    for title, stamps in reqs:
        lmin = min([lmin] + stamps)
        lmax = max([lmax] + stamps)
        d = (title, stamps)
        ds.append(d)
    with open('ønsker.json', 'w') as f:
        json.dump((lmin, lmax, ds), f)

if __name__ == '__main__':
    run()
