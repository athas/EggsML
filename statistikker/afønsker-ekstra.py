#!/usr/bin/env python3

import subprocess
import itertools
import sys

def run(filepath = None):
    pairs = [(int(stamp), title) for stamp, title in
             (line.split(' ', 1) for line in
              subprocess.check_output(['./afønsker.sh']).decode('utf-8').strip().split('\n'))]
    pairs.sort(key=lambda k: (k[1], k[0]))
    reqs = [(name, [stamp for stamp, title in sub_pairs])
            for name, sub_pairs in itertools.groupby(pairs, key=lambda k: k[1])]
    reqs.sort(key=lambda k: len(k[1]))
    reqs = list(filter(lambda k: len(k[1]) > 1, reqs))[::-1]

    if filepath is not None:
        with open(filepath, 'w') as f:
            for title, stamps in reqs:
                print(title, file=f)
                print(stamps, file=f)
                print("", file=f)
            f.close()
    else:
        for title, stamps in reqs:
            print(title, file=f)
            print(stamps, file=f)
            print("", file=f)

if __name__ == '__main__':
    run(sys.argv[1])
