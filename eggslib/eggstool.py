#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
import sys

def print_aliases(eggs, name):
    for user_aliases in eggs.aliases:
        if name.lower() in map(lambda (x): x.lower(), user_aliases):
            for alias in user_aliases:
                print alias
            return True
    return False

def print_wishes(eggs):
    for wish in eggs.get_wishes():
        print wish

def print_balances(eggs):
    userinfo = eggs.get_userinfo()
    for v in userinfo:
        print round(userinfo[v]['balance'], 2), eggs.get_alias_rand(v)

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
        if not print_aliases(e, sys.argv[3]):
            exit(1)
    elif command == "wishes":
        print_wishes(e)
    elif command == "balances":
        print_balances(e)
    else:
        exit("Unrecognized command %s" % command)
