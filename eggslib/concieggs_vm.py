#!/usr/bin/env python

import subprocess
from subprocess import Popen
import os
from time import time

#DIR = os.path.join(os.getcwd(),'concieggs')
DIR = '/eggsml/concieggs'
CMDDIR  = os.path.join(DIR,'cmds')
DBDIR  = os.path.join(DIR,'db')
      
ENV = {'CONCIEGGS_DIR' : DIR 
      ,'CONCIEGGS_DB_DIR' : DBDIR
      ,'EGGS_DIR'         : os.path.join(DIR,'..')
      ,'EGGS_LIB_DIR'     : os.path.join(DIR,'..','eggslib')
      }
#Intet kan antages
PATH = os.path.join(DIR,'eggspi')+":/bin:/usr/bin"
import logging
logging.basicConfig(level=logging.DEBUG)

def run(cmd,user,args=None):
  if args is None: args=[] #
  os.environ.update(ENV)
  os.environ['EGGS_USER'] = user
  os.environ['EGGS_LINE'] = 'dummy'
  os.environ['PATH'] = PATH
  cmdline = os.path.join(CMDDIR,cmd)
  args = [str(int(time()))]
  process = Popen([cmdline]+args,stdout=subprocess.PIPE)
  output, unused_err = process.communicate() 
  retcode = process.poll()

  if retcode:
      return ("failed retcode[%s] output:[%s]" %(retcode,) )
  else:
      return output

  
def neggst():
  return run('neggst','dummy').replace("\n", "<br />\n");

def preggs():
  return run('preggs','dummy').replace("\n", "<br />\n");


def test():
  print neggst()

if __name__ == '__main__':
  test()
