#!/usr/bin/env python
#-*- encoding: utf-8 -*-
from __future__ import with_statement
#I'm not in shell, i'm such a happy bot, really!

import sys
import logging
from eggslib.eggsml import eggsml
from sets import Set
logging.basicConfig(level=logging.DEBUG)

import simplejson as json

from pyjabberbot import PersistentJabberBot,botcmd

import datetime,os
import subprocess,shlex
import fileinput

FILE = 'slashdotfrokost'
REMOTE = "git@github.com:Athas/EggsML.git"
BRANCH = "master" 
#We could do git checkout file on error
def commit(eggsers):
  '''  
    eggsers, Set of eggsers
  '''
  gitpull   = ['git','pull',REMOTE,BRANCH]
  subprocess.call(gitpull)

  date = datetime.date.today()
  eaters = ", ".join(eggsers)  
  meal =  "%s, %s" % (date,eaters)
  fname = os.path.join(os.getcwd(),FILE)
  #can crash, but who cares with git
  for line in fileinput.input(fname,inplace=1):
     if "::MÅLTIDSDATA::" == line.strip():
        print line,
        print meal
     else:  
       print line,
  gitcommit = ['git','commit','slashdotfrokost','-m',meal]
  subprocess.call(gitcommit)
  gitpush  = ['git','push',REMOTE,BRANCH] 
  subprocess.call(gitpush)

GITFETCH =['git','fetch',REMOTE]
GITMERGE = ['git','merge','origin',BRANCH]
  
def git_fetch():
  ''' '''
  try:
      return check_output(GITFETCH)
  except subprocess.CalledProcessError,e:
      return "Kommando fejlede, returkode [%s]\n%s" % (e.returncode,e.output)

def git_merge():
  ''' '''
  try:
      return check_output(GITMERGE,stderr=subprocess.STDOUT)
  except subprocess.CalledProcessError,e:
      return "Kommando fejlede, returkode [%s]\n%s" % (e.returncode,e.output)



def git_commit_and_push(file):
  ''' Commit and push a file'''
  gitcommit = ['git','commit',file,'-m',"Commit by eggtimer"]
  commitout = check_output(gitcommit)
  
  gitpush  = ['git','push',REMOTE,BRANCH] 
  pushout  = check_output(gitpush)
  return (commitout,pushout)


  
###conciegs emu####

#from py2.7/py3.1
class CalledProcessError(Exception):
  """This exception is raised when a process run by check_call() or
  check_output() returns a non-zero exit status.
  The exit status will be stored in the returncode attribute;
  check_output() will also store the output in the output attribute.
  """
  def __init__(self, returncode, cmd, output=None):
     self.returncode = returncode
     self.cmd = cmd
     self.output = output
     def __str__(self):
       return "Command '%s' returned non-zero exit status %d" % (self.cmd, self.returncode)

def check_output(*popenargs, **kwargs):
    if 'stdout' in kwargs:
        raise ValueError('stdout argument not allowed, it will be overridden.')
    process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
    output, unused_err = process.communicate()
    print "UUSED_ERROR",unused_err
    retcode = process.poll()
    if retcode:
        cmd = kwargs.get("args")
        if cmd is None:
            cmd = popenargs[0]
        raise CalledProcessError(retcode, cmd, output=output)
    return output


DIR = os.path.join(os.getcwd(),'concieggs')
CMDDIR  = os.path.join(DIR,'cmds')
DBDIR  = os.path.join(DIR,'db')
ENV = {'CONCIEGGS_DIR' : DIR 
      ,'CONCIEGGS_DB_DIR' : DBDIR
      ,'EGGS_DIR'         : os.path.join(DIR,'..')
      ,'EGGS_LIB_DIR'     : os.path.join(DIR,'..','eggslib')
      }

import re
#concieggs emu
  
  
### jid map handlers ###
JIDDB = os.path.join(DBDIR,'jid2egg')

def rm_jid(jid):
  val = None
  with open(JIDDB,'r') as f:
    d = json.load(f)
    val = d.pop(jid,None)

  with open(JIDDB,'w+') as f:
    json.dump(d,f,indent="")
  return val 

def add_jid(jid,eggname):
  """docstring for add_jid"""
  d = {} 
  with open(JIDDB,'r') as f:
    d = json.load(f)
    d[jid] = eggname

  with open(JIDDB,'w+') as f:
    json.dump(d,f,indent="")

def jid_whois(jid):
  with open(JIDDB,'r') as f:
    d = json.load(f)
    return d.get(jid,None)

def add_nick(user):
  """docstring for add_nick"""
  pass

   
  
class EggTimer(PersistentJabberBot):
  """docstring for EggTimer"""
  def __init__(self, jid, password, res = None):
    super(EggTimer, self).__init__(jid, password, res)
    self.eggsml = eggsml()
    self.eggsml.parse("slashdotfrokost")
    #simplest bot possible
    self.lunch = Set()
    # map from room jid to bare jid
    self.fulljids = {} 

  #def jid2alias(self,jid):
    #"""docstring for _jid2alias"""
    #lookfor = JID2ALIAS.get(jid,None)
    #if lookfor == None: return None
    #return None
 
  def _concieggs(self,user,line):
    self.log.debug("CONCIEGGS user[%s],line[%s]" % (user,line))
    if user==None:
      return "who are you?"

    eggenv = ENV.copy()
    eggenv['EGGS_USER'] = user
    eggenv['EGGS_LINE']  = line.encode('utf-8')
    m = re.match("(\w+)\s?(.*)",line,re.U)
    if m:
      cmd = m.group(1)
      args = m.group(2)
      exe = [os.path.join(CMDDIR,cmd)] + [args]
      try:
        out = check_output(exe,env=eggenv)
        return out
      except OSError:
        return "%s: Du bad mig om [%s], men den kommando har jeg ikke!" % (user,cmd)
      except CalledProcessError,e:
        print e.returncode
        return "Kommandoen fejlede [%s]!  Prøv at spørge mig om 'udu'." % (e.returncode)
#      except UnicodeEncodeError, e:
#        self.log.debug("unicodeerror [%s]" % e)
#        raise e
#        return "I have an issue with unicode"
      except TypeError, e:
        self.log.debug("TYPEERROR %s" % e)
        return "python fails"
      
    return "not a command"  

  def _get_eggname(self,nick):
    '''returns a string with eggname'''
    privjid = self.fulljids[nick]
    eggname = jid_whois(privjid)
    return eggname

   
  def _lunch_add_egg(self,eggname):
    eggname = eggname.lower() 
    if eggname in self.lunch:
        return ("Only one eggs you can have %s" % eggname)
    else:
        self.log.debug("adding [%s]" % eggname)
        self.lunch.add(eggname)
        alias =  self.eggsml.get_alias_rand(eggname)
        return ("Pay for eggs you will %s" % (alias))

  @botcmd(name="!fetch")
  def fetch(self,msg,args):
    return git_fetch() 
  
  @botcmd(name="!merge")
  def merge(self,msg,args):
    return git_merge()

  @botcmd(name="!est")
  def add_guest(self,msg,args):
    """!eggsguest <eggpayer> <guest>"""
    return "no eggguests yet"

  @botcmd(name="!eggsother")
  def add_egg_fixed(self,msg,args):
    """!eggsother <eggname>"""
    eggname = args.strip()
    for aliaslist in eggsml.aliases:
      if eggname in aliaslist:
        return self._lunch_add_egg(aliaslist[0])
    
    return ("No such egg [%s]" % eggname)
    #No check for now 

  @botcmd(name="!eggsno")
  def add_rm_fixed(self,msg,args):
    '''!eggsno <egg> (removes egg)'''
    eggname = args.strip()
    if eggname in self.lunch:
      self.lunch.remove(eggname)
      return "Egg %s starves" % self.eggsml.get_alias_rand(eggname)

    return ("No such egg [%s]" % eggname)

  @botcmd(name="!eggsme")
  def add_egg(self, msg, args):
    """docstring for adduser"""
    nick = msg.getFrom().getResource()
    eggname = self._get_eggname(nick)
    if eggname:
      return self._lunch_add_egg(eggname)
    return ("Who are you! %s",eggname)

  @botcmd(name="!eggstat")
  def stat(self,msg,args):
    """Shows eggsmlers for today"""
    users = [self.eggsml.get_alias_rand(egg) for egg in self.lunch]
    return  ("%s eggsmlers today %s" % ( len(self.lunch),",".join(users) ) )
 
  @botcmd(name="!eggsdone")
  def clear(self, msg, args):
    """docstring for reset"""
    s = "eggs that ate eggs %s" % ([egg for egg in self.lunch])
    commit(self.lunch)
    self.lunch.clear()
    return s

  @botcmd(name="!whoami")
  def whoami(self,msg,args):
    """docstring for whoami"""
    nick = msg.getFrom().getResource()
    privjid = self.fulljids[nick]

    eggname = jid_whois(privjid)
    if eggname:
      return "Hi! %s (%s)" % (eggname,privjid) 
    else:
      return "not an egg %s? (!eggname %s)" % (privjid,EggTimer.eggjid.__doc__) 

  @botcmd(name="!eggname")
  def eggjid(self,msg,args):
    """nick [jid]"""
    #FIXME check if nick exists
    m = re.match("(\w+)(\s(.+@.+))?",args,re.U)
    if m:
      (eggname,privjid,privjid2) = m.groups()
      print "MATCHED", m.groups()
#      self.log.debug("MATHCED %s" % ",".join(m.groups()))
      if privjid==None:
        nick = msg.getFrom().getResource()
        privjid = self.fulljids.get(nick)
      if privjid: 
        add_jid(privjid,eggname)
      return "Hi! %s (%s) (%s)" % (eggname,privjid,privjid2) 
    else:
      return EggTimer.eggjid.__doc__

  
  @botcmd(name="!jids")
  def jids(self,msg,args):
    """docstring for jids2"""
    return str(self.fulljids)

  @botcmd(name="!nexteggs")
  def set_next_lunch(self,msg,args):
    """docstring for setNextLunchTime"""
    return "eggstimer does not grasp time"

  @botcmd(name="egg:")
  def concieggs(self,msg,args):
    """Do whatever concieggs does"""
    nick = msg.getFrom().getResource()
    eggname = self._get_eggname(nick)
    if eggname == None:
      eggname = nick
    return self._concieggs(eggname,args) 
    
  '''
  We need a map priv jid to map, it's unique,chatroom nick/resource isn't.
  Further some clients it's hard to set (impossible?) to set nick/resource
  Anoyingly we need state to handle this, because 
  groupchat messages only contain chatroom jid/nick (reflected by server)
  For every Presence we just update the map chat nick->priv jid (both text) 
  '''
  def callback_presence(self,conn,pre,
              status_type_changed = None,status_msg_changed = None):    
    """doccstring for callback presence"""
    #yay oo crap
    super(EggTimer, self).callback_presence(conn, pre)

    #Create mapping for group chat resource to private jid
    nick =  pre.getFrom().getResource()
    privjid =  pre.getJid() #presence>x>item#jid
    print  "TYPE",privjid
    #update nick->jid
    if privjid:
      priv,_ = privjid.split("/")
      self.log.debug("fulljids[%s] = %s" % (nick,priv))
      self.fulljids[nick] =  priv
 
  #@botcmd
  #def hello(self,msg,args):
    #"""docstring for hello"""
    #user = msg.getFrom()
    #return "hello [%s]" % user

def main():
  if len(sys.argv) < 4:
    print 'Usage: %s login@host password room <resource>' % sys.argv[0]
    sys.exit(1)
  if len(sys.argv) >= 5:
    resource = sys.argv[4]
  else:
    resource = None

  bot = EggTimer(sys.argv[1],sys.argv[2],res=resource)

  bot.debug_heartbeat = False
  bot.syn_interval = 30

  room = sys.argv[3]
  
  def join():
    bot.log.debug("Joining ROOM [%s]" % room)
    bot.join_room(room)
    bot.send(room, 'æææggscellent', 'groupchat')
     
  
  bot.on_connect = join
  bot.on_reconnect = join
  bot.serve_forever()


if __name__ == '__main__':
  main()
