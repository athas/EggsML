#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
from toollib import *
import socket
import sys
import os

if __name__ == '__main__':
    if len(sys.argv) != 3:
        exit("Usage: %s <lunchfile> <socket-filename>" % sys.argv[0])
    lunchfile = sys.argv[1]
    lunchfile_changetime = os.stat(lunchfile).st_mtime
    filename = sys.argv[2]
    e = eggsml()
    e.parse(lunchfile)
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.bind(filename)
    s.listen(1)
    while True:
        conn, addr = s.accept()
        fd = conn.makefile()
        # See if we need to reparse the lunchfile...
        statinfo = os.stat(lunchfile)
        if statinfo.st_mtime != lunchfile_changetime:
            e.parse(lunchfile)
            sys.stderr.write(str(e.aliases))
            lunchfile_changetime = statinfo.st_mtime
        conn.close() # fd keeps the socket alive.
        sys.stdout = fd
        words = fd.readline().split()
        if len(words) == 0:
            print "No command!"
        else:
            command = words[0]
            if command == "aliases":
                if len(words) < 1:
                    sys.stderr.write("Usage: %s <alias1> [more aliases...]\n" % (command,))
                else:
                    for i in range(len(words)-1):
                        if not print_aliases(e, words[i+1]):
                            continue
                        if i != len(words)-2:
                            print
            elif command == "wishes":
                print_wishes(e)
            elif command == "balances":
                print_balances(e)
            elif command == "lunches":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    continue
                else:
                    print_lunches(e, words[1])
            elif command == "eggsmates":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    continue
                else:
                    print_eggsmates(e, words[1])
            elif command == "eggscount":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    continue
                else:
                    print_eggscount(e, words[1])
            elif command == "consecutive":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    continue
                else:
                    print_consecutive(e, words[1])
            elif command == "cmpnames":
                if len(words) != 3:
                    sys.stderr.write("Usage: %s <name> <name>" % (command,))
                else:
                    same_eggser(e, words[1], words[2])
            else:
                sys.stderr.write("Unrecognized command %s" % command)
        fd.close()
