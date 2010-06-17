#!/usr/bin/env python
# -*- encoding: utf-8 -*-

from os import system

class update:
    
    def __init__(self):
        t = open('template.html')
        temp = t.read()
        t.close()

        retval = system("cd /eggsml; git pull > /dev/null")
        content = "Return value: " + str(retval)
        print "Content-type: text/html; charset=UTF-8\n"
        print temp.replace('{{CONTENT}}', content)

update()
