#!/usr/bin/env python
# -*- encoding: utf-8 -*-

from os import system, geteuid
import sys

class update:
    def __init__(self):
        t = open('template.html')
        temp = t.read()
        t.close()

        retval = system("cd /eggsml; umask 002; git pull > /dev/null")
        content = "Return value: " + str(retval)
        print "Content-type: text/html; charset=UTF-8\n"
        print temp.replace('{{CONTENT}}', content)

update()
