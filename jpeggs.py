#!/usr/bin/env python
# -*- encoding: utf-8 -*-

import os
import pyexiv2

from server import server

class index:
    
    def __init__(self):
        t = open('template.html')
        temp = t.read()
        t.close()
        jpeggdir = "jpeggs"
        content = ""
        errorlog = ""

        kindereggsize = "256x192"

        s = server()
        colleggtion = s.get_get_data("colleggtion")

        colleggtion_path = os.path.join(jpeggdir,colleggtion)
        if colleggtion != "" and os.path.isdir(colleggtion_path):
            content += "<h1>" + colleggtion + "</h1>\n"
            jpeggs = [d for d in os.listdir(colleggtion_path) 
                      if os.path.isfile(os.path.join(colleggtion_path,d))
                      and d.endswith(".jpegg")
                      and not d.endswith(".kinder.jpegg")]
            
            jpegg_html_colleggtion = {}

            for jpegg in jpeggs:
                jpegg_html = ""
                kinderegg = jpegg[:-6] + ".kinder.jpegg"
                kinderegg_path = os.path.join(colleggtion_path, kinderegg)
                jpegg_path = os.path.join(colleggtion_path, jpegg)
                
                # Read EggsIF
                metadata = pyexiv2.Image(jpegg_path)
                metadata.readMetadata()
                try:
                    date = metadata["Exif.Image.DateTime"]
                except KeyError:
                    date = date.fromtimestamp(0)
                try:
                    descr = metadata["Exif.Photo.UserComment"]
                except KeyError:
                    try:
                        descr = metadata["Exif.Image.ImageDescription"]
                    except KeyError:
                        descr = "eggs?"
                
                if not os.path.isfile(kinderegg_path):
                    cmd = 'convert "' + jpegg_path + '" -resize ' + kindereggsize + '\> "' + kinderegg_path + '"'
                    retval = os.system(cmd)
                    if retval != 0:
                        errorlog += "Couldn't create kinderegg for '" + jpegg_path + "' (ImageMagick return code: " + str(retval) + ")"
                
                jpegg_html += '\t<a href="'  + jpegg_path + '" class="jpegg">'
                jpegg_html += '<div><img src="'+ kinderegg_path + '" title="'+ descr +'" /><br />'+descr+'</div></a>\n'

                jpegg_html_colleggtion[date] = jpegg_html

            content += '<div id="colleggtion">\n'
            for date, html in sorted(jpegg_html_colleggtion.items(), reverse=True, key=lambda x: x[0]):
                content += html
            content += "</div>"
            
        else:
            content += "<h1>Jpegg colleggtion</h1>\n"
            colleggtions = [d for d in os.listdir(jpeggdir)
                            if os.path.isdir(os.path.join(jpeggdir,d))]
            
            content += "<ul>\n"
            for g in colleggtions:
                content += "\t<li>"
                content += '<a href="?colleggtion=' + g + '">' + g + '</a>'
                content += "</li>\n"
                
            content += "</ul>"
         
        print "Content-type: text/html; charset=UTF-8\n"
        print temp.replace('{{CONTENT}}', content + errorlog)

index()
