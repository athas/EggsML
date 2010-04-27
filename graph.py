#!/usr/bin/env python
# -*- encoding: utf-8 -*-

from eggsml import eggsml
import random

class graph:
    e = None
    svg = ''
    width = 610
    height = 50
    gwidth = 600
    gheight = 50
    padding = [0, 5, 0, 5]
    colours = None

    def columns(self):
        count = self.e.get_count()
        total = 0
        for c in count:
            total += count[c]
        cwidth = self.gwidth/total
        rects = '<g id="rects">\n'
        texts = '<g id="labels">\n'
        numbers = '<g id="values">\n'
        x = self.padding[3]
        y = self.padding[0]
        ah = 25
        toptext = '<text x="%s" y="%s" style="font-size: 18px;">%s</text>\n' % (self.width/2-len(str(total))*3, self.padding[0], total)
        n = sorted(count.iteritems(), key=lambda (k,v):(v,k), reverse=True)
        #n.sort(lambda x,y : x[1] < y[1])
        for name in n:
            w = cwidth*name[1]
            alias = self.e.get_alias(name[0], w/4)
            a = '<rect x="%s" y="%s" width="%s" height="%s" style="fill:%s;" />\n' % (x, y, w, ah, self.get_colour(self.e.get_alias(alias)))
            t = '<text x="%s" y="%s">%s</text>\n' % (x+w/2-len(alias)*3, y+ah/2+5, alias)
            l = '<text x="%s" y="%s">%s</text>\n' % (x+w/2-len(str(name[1]))*3, y+ah+14, name[1])
            rects += a
            texts += t
            numbers += l
            x += w
        rects += '</g>\n'
        texts += '</g>\n'
        numbers += '</g>\n'
        svg = '%s\n%s\n%s\n%s\n' % (toptext, rects, texts, numbers)
        return svg
    
    def get_colour(self, alias):
        if self.colours==None:
            self.colours = self.e.get_colours()
        try:
            colour = self.colours[alias]
            return colour
        except:
            return 'rgb(%s, %s, %s)' % (random.randint(0,200), random.randint(0,200), random.randint(0,200))
    
    def make_svg(self):
        svg = '''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:cc="http://creativecommons.org/ns#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:svg="http://www.w3.org/2000/svg"
    xmlns="http://www.w3.org/2000/svg"
    width="%s"
    height="%s">
<style text="text/css">
text {
    font-family: helvetica, verdana, tahoma, sans;
}
#labels text {
    fill: #ffffff;
    stroke-width: 1px;
}
</style>
''' % (self.width, self.height)
        svg += self.columns()
        svg += '</svg>'
        self.svg = svg

    def __init__(self):
        self.e = eggsml()
        self.e.parse('slashdotfrokost')
        self.make_svg()
#        print "Content-type: image/svg+xml; charset=UTF-8\n"
        print self.svg

graph()
