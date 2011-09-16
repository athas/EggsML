#!/usr/bin/env python

import subprocess
from subprocess import Popen
import os

#DIR = os.path.join(os.getcwd(),'concieggs')
DIR = '/eggsml/concieggs/'
CMDDIR  = os.path.join(DIR,'cmds')
DBDIR  = os.path.join(DIR,'db')
      

ENV = {'CONCIEGGS_DIR' : DIR 
      ,'CONCIEGGS_DB_DIR' : DBDIR
      ,'EGGS_DIR'         : os.path.join(DIR,'..')
      ,'EGGS_LIB_DIR'     : os.path.join(DIR,'..','eggslib')
      }

import logging
logging.basicConfig(level=logging.DEBUG)

def run(cmd,user):
  os.environ.update(ENV)
  os.environ['EGGS_USER'] = user
  os.environ['EGGS_LINE'] = 'dummy'
  cmdline = os.path.join(CMDDIR,cmd)
  
  process = Popen(cmdline,stdout=subprocess.PIPE)
  output, unused_err = process.communicate() 
  retcode = process.poll()

  if retcode:
      return ("failed retcode[%s] output:[%s]" %(retcode,) )
  else:
      return output

  
def neggst():
  return run('neggst','dummy') 

def test():
  print neggst()

if __name__ == '__main__':
  test()
