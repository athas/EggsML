#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
from toollib import *
from random import choice
import sys

if __name__ == '__main__':
    e = eggsml()
    if len(sys.argv) < 3:
        exit("Usage: %s <lunchfile> <command>" % sys.argv[0])
    lunchfile = sys.argv[1]
    command = sys.argv[2]
    e.parse(lunchfile)
    if command == "aliases":
        if len(sys.argv) < 4:
            exit("Usage: %s %s %s <alias1> [more aliases...]" % (sys.argv[0], lunchfile, command))
        for i in range(len(sys.argv)-3):
            if not print_aliases(e, sys.argv[i+3]):
                exit(1)
            if i != len(sys.argv)-4:
                print
    elif command == "wishes":
        print_wishes(e)
    elif command == "balances":
        print_balances(e)
#    elif command == "balance_of_payments":
#        print_balance_of_payments(e)
    elif command == "lunches":
        if len(sys.argv) != 4:
            exit("Usage: %s %s %s <alias>" % (sys.argv[0], lunchfile, command))
        if not print_lunches(e, sys.argv[3]):
            exit(1)
    elif command == "eggsmates":
        if len(sys.argv) != 4:
            exit("Usage: %s %s %s <alias>" % (sys.argv[0], lunchfile, command))
        if not print_eggsmates(e, sys.argv[3]):
            exit(1)
    elif command == "eggscount":
        if len(sys.argv) != 4:
            exit("Usage: %s %s %s <alias>" % (sys.argv[0], lunchfile, command))
        if not print_eggscount(e, sys.argv[3]):
            exit(1)
    elif command == "consecutive":
        if len(sys.argv) != 4:
            exit("Usage: %s %s %s <alias>" % (sys.argv[0], lunchfile, command))
        if not print_consecutive(e, sys.argv[3]):
            exit(1)
    elif command == "allaliases":
        print_all_aliases(e)
    elif command == "cmpnames":
        if len(sys.argv) != 5:
            exit("Usage: %s %s %s <name> <name>" % (sys.argv[0], lunchfile, command))
        if same_eggser(e, sys.argv[3], sys.argv[4]):
            exit(0)
        else:
            exit(1)
    else:
        exit("Unrecognized command %s" % command)
