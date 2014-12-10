# -*- coding: utf-8 -*-

'''
Accessors to concieggs' eggspi shell commands.

All accessors return a tuple (output, return code).
'''

from eggsthon import *

import subprocess
import os
import StringIO


class RetCode(object):
    def __init__(self, retcode):
        self.retcode = retcode

    def __bool__(self):
        return self.retcode == 0

    def __int__(self):
        return self.retcode

    def __str__(self):
        return str(self.retcode)
        
    def __repr__(self):
        return str(self.retcode)

def make_cmd_fun(cmdname):
    def f(*args, **kwds):
        stdin = kwds.get('stdin')
        args_all = [cmdname] + list(args)
        try:
            if stdin is None:
                ret = subprocess.check_output(args_all)
            else:
                ret = subprocess.check_output(
                    args_all, stdin=StringIO.StringIO(stdin))
            if ret == '':
                ret = True
            return ret
        except subprocess.CalledProcessError as e:
            return RetCode(e.returncode)
    return f

for f in os.listdir(CONCIEGGS_LIB_DIR):
    globals()[f] = make_cmd_fun(f)
