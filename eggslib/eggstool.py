#!/usr/bin/env python
# encoding: utf8

from eggsml import eggsml
from random import choice
import sys

def user_aliases(eggs, name):
    for user_aliases in eggs.aliases:
        if name.lower() in map(lambda (x): x.lower(), user_aliases):
            return user_aliases

def print_aliases(eggs, name):
    aliases = user_aliases(eggs,name)
    if aliases != None:
        for alias in aliases:
            print alias
        return True
    return False

def print_wishes(eggs):
    for wish in eggs.get_wishes():
        print wish

def print_balances(eggs):
    userinfo = eggs.get_userinfo()
    for v in userinfo:
        print round(userinfo[v]['balance'], 2), v


from datetime import timedelta
def nextday(ds,d):
  if ds == []:
    return True
  else:
    return (ds[-1] + timedelta(days=1) == d)
    

def print_consecutive(eggs,name):
  '''Eeek
  '''
  dates = e.get_dates()
  aliases = user_aliases(eggs,name)
  if aliases == None:
    return False
  n = aliases[0]
  #acc = {}
  acc = ([],[]) 
  for d in dates:
    for eggs in d['users']:
      if eggs['user'] == n:
       (current,longest) = acc 
       if nextday(current,d['date']):
          current.append(d['date'])
       else:
         acc = ([],current) if len(current) > len(longest) else ([],longest)
 
  if len(longest) > len(current):
    print len(longest)
  else:
    print len(current)

def print_lunches(eggs, name):
    dates = eggs.get_dates()
    aliases = user_aliases(eggs,name)
    if aliases == None:
        return False
    for d in reversed(dates):
        for u in d['users']:
            if u['user'] in aliases:
                print d['date']

def print_eggsmates(eggs,name):
    
  aliases = user_aliases(eggs,name) 
  if aliases == None:
    return False

  def ate(lunch,user):
      for x in lunch:
          if x['user']==aliases[0]:
              return True
      return False

  acc = {}
  for l in eggs.dates: 
      if ate(l['users'],aliases[0]):
        for x in l['users']:
          if x['user'] <> aliases[0]:
              acc[x['user']] = acc.get(x['user'],0) + 1

  eggsmates = sorted(acc.items(),key=lambda i: i[1],reverse=True)
 
  s = choice(aliases) + 's BFFs,'
  for (user,count) in eggsmates[0:5]:
      s += " %s (%s)" % (choice(user_aliases(eggs,user)),count)

  print s
  return True

def print_eggscount(eggs,name):
    '''[{date: ts ,users: [ {amount:int, user:string} ] }]  -> int
    '''
    aliases  = user_aliases(eggs,name)
    if aliases == None:
        return False
    
    def inner(items):
      #swap x['amount'] with '1' to count partitipation 
      return reduce(lambda acc,x: (x['amount'] if x['user']==aliases[0] else 0)+acc,items,0)
    
    print reduce(lambda acc,x: inner(x['users'])+acc,eggs.dates,0)

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

    else:
        exit("Unrecognized command %s" % command)
