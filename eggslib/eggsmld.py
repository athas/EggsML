#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
from toollib import *
import StringIO
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
        sys.stdout = StringIO.StringIO()
        exit = 0
        words = fd.readline().split()
        if len(words) == 0:
            print "No command!"
        else:
            command = words[0]
            if command == "aliases":
                if len(words) < 1:
                    sys.stderr.write("Usage: %s <alias1> [more aliases...]\n" % (command,))
                    exit = 1
                else:
                    for i in range(len(words)-1):
                        if not print_aliases(e, words[i+1]):
                            exit = 1
                        elif i != len(words)-2:
                            print
            elif command == "wishes":
                print_wishes(e)
            elif command == "balances":
                print_balances(e)
            elif command == "lunches":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    exit = 1
                else:
                    if not print_lunches(e, words[1]):
                        exit = 1
            elif command == "eggsmates":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    exit = 1
                else:
                    if not print_eggsmates(e, words[1]):
                        exit = 1
            elif command == "eggscount":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    exit = 1
                else:
                    print_eggscount(e, words[1])
            elif command == "consecutive":
                if len(words) != 2:
                    sys.stderr.write("Usage: %s <alias>" % (command,))
                    exit = 1
                else:
                    if not print_consecutive(e, words[1]):
                        exit = 1
            elif command == "cmpnames":
                if len(words) != 3:
                    sys.stderr.write("Usage: %s <name> <name>" % (command,))
                else:
                    if (same_eggser(e, words[1], words[2])):
                        exit = 0
                    else:
                        exit = 1
            else:
                sys.stderr.write("Unrecognized command %s" % command)
        fd.write(str(int(exit))+'\n')
        fd.write(sys.stdout.getvalue())
        fd.close()
