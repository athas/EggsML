#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
import sys

def print_aliases(eggs, name):
    for user_aliases in eggs.aliases:
        if name in user_aliases:
            for alias in user_aliases:
                print alias
            return

if __name__ == '__main__':
    e = eggsml()
    if len(sys.argv) < 3:
        exit("Usage: %s <lunchfile> <command>" % sys.argv[0])
    lunchfile = sys.argv[1]
    command = sys.argv[2]
    e.parse(lunchfile)
    if command == "aliases":
        if len(sys.argv) != 4:
            exit("Usage: %s %s %s <alias>" % (sys.argv[0], lunchfile, command))
        print_aliases(e, sys.argv[3])
    else:
        exit("Unrecognized command %s" % command)
