#!/usr/bin/env python
# -*- encoding: utf-8 -*-

#I'm not in shell, i'm such a happy bot

import sys
import logging
import random
from eggsml import eggsml
from sets import Set
logging.basicConfig(level=logging.DEBUG)
 
from pyjabberbot import PersistentJabberBot,botcmd

#We don't have full/bare jid for know, only private. Boring xmpp stuff to figure out

JID2ALIAS =  {'eggs@conference.jabber.dk/Frej Soya' : 'frej'
              ,'eggs@conference.jabber.dk/Athas' : 'troels'
              ,'eggs@conference.jabber.dk/Dybber' : 'dybber'
              ,'eggs@conference.jabber.dk/BP'     : 'bp'
              ,'eggs@conference.jabber.dk/live4adrenalin' : 'henne'
              }



class EggTimer(PersistentJabberBot):
  """docstring for EggTimer"""
  def __init__(self, jid, password, res = None):
    super(EggTimer, self).__init__(jid, password, res)
    self.eggs = eggsml() 
    self.eggs.parse("slashdotfrokost")

    #simplest bot possible
    self.users = Set()
  def jid2alias(self,jid):
    """docstring for _jid2alias"""
    lookfor = JID2ALIAS.get(jid,None)
    if lookfor == None: return None
    #ugly aliaslist, should be a dict of key,aliases,
    for aliaslist in eggsml.aliases:
      head = aliaslist[0]
      print "HEAD", head
      if head==lookfor:
        print "ALIASLIST: ", aliaslist
        return random.choice(aliaslist)
    return None


  @botcmd(name="!nexteggs")
  def set_next_lunch(self):
    """docstring for setNextLunchTime"""
    return "eggstimer does not grasp time"

  
  @botcmd(name="!eggsme")
  def add_egg(self, msg, args):
    """docstring for adduser"""
    print "MSG",msg
    user = msg.getFrom()
    if user in self.users:
        return ("Only one eggs you can have [%s]" % user)
    else:
        self.log.debug("adding [%s]" % user)
        self.users.add(str(user))
        print "USERS",self.users
        alias =  self.jid2alias(user)
        return ("Pay for eggs you will %s" % (alias))

  @botcmd(name="!eggstat")
  def stat(self,msg,args):
    """Shows eggsmlers for today"""
    users = [self.jid2alias(u) for u in self.users]
    return  ("%s eggsmlers today %s" % ( len(self.users),",".join(users) ) )


  @botcmd(name="!eggsdone")
  def clear(self, msg, args):
    """docstring for reset"""
    s = "eggs that ate eggs [%s]" % self.users
    self.users.clear()
    return s

  #@botcmd
  #def hello(self,msg,args):
    #"""docstring for hello"""
    #user = msg.getFrom()
    #return "hello [%s]" % user


def main():
  if len(sys.argv) < 4:
    print 'Usage: %s login@host password room' % sys.argv[0]
    sys.exit(1)
 
  bot = EggTimer(sys.argv[1],sys.argv[2])

  bot.debug_heartbeat = False
  bot.syn_interval = 5

  room = sys.argv[3]
  
  def join():
    bot.join_room(room)
    bot.send(room, 'æææggscellent', 'groupchat')
    
  
  bot.on_connect = join
  bot.serve_forever()


if __name__ == '__main__':
  main()
