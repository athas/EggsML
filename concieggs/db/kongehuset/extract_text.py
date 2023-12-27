#!/usr/bin/env python3
#
# Run this program to extract data from metadata.json to contents.json.

import collections
import json
import time
from html.parser import HTMLParser
from html.entities import name2codepoint

import requests

Item = collections.namedtuple('Item', ['url', 'title', 'type'])
Content = collections.namedtuple('Content', ['title', 'paragraphs'])

class KongehusetHTMLParser(HTMLParser):
    def prepare(self):
        self.inside_container = False
        self.inside_content = False
        self.paragraphs = []
        self.current_paragraph = ''

    def handle_starttag(self, tag, attrs):
        if tag == 'div' and len(attrs) > 0 and attrs[0][0] == 'class' and attrs[0][1] == 'rich-text__container__content':
            self.inside_container = True
        elif self.inside_container and tag == 'p' and len(attrs) == 0:
            self.inside_content = True
        elif self.inside_content:
            self.inside_content = False
            self.current_paragraph = ''

    def handle_endtag(self, tag):
        if tag == 'div' and self.inside_container:
            self.inside_container = False
        elif tag == 'p' and self.inside_content:
            self.inside_content = False
            p = self.current_paragraph.replace('\xa0', '') # Ignore non-breaking space
            if len(p) > 0 and not p.endswith(':'):
                self.paragraphs.append(p)
            self.current_paragraph = ''

    def handle_data(self, data):
        if self.inside_content:
            self.current_paragraph += data

    def handle_entityref(self, name):
        if self.inside_content:
            c = chr(name2codepoint[name])
            self.current_paragraph += c

    def handle_charref(self, name):
        if self.inside_content:
            if name.startswith('x'):
                c = chr(int(name[1:], 16))
            else:
                c = chr(int(name))
            self.current_paragraph += c

kongehuset_parser = KongehusetHTMLParser()

def get_html(url):
    req = requests.get('https://www.kongehuset.dk' + url)
    return req.text

def parse_html(html):
    kongehuset_parser.prepare()
    kongehuset_parser.feed(html)
    kongehuset_parser.close()
    return kongehuset_parser.paragraphs

with open('metadata.json', 'r') as f:
    items = json.load(f)
items = (Item(*args) for args in items)
# Types: '', 'Galleri', 'Indhold', 'Nyhed', 'Pressemeddelelse', 'Programpunkt', 'Tale', 'Video'
items = [item for item in items if item.type == 'Indhold']
n_items = len(items)

contents = []
for item, i in zip(items, range(1, n_items + 1)):
    print(f'{i}/{n_items}')
    html = get_html(item.url)
    paragraphs = parse_html(html)
    if len(paragraphs) > 0:
        content = Content(title=item.title, paragraphs=paragraphs)
        contents.append(content)
    time.sleep(0.05)

with open('contents.json', 'w') as f:
    json.dump(contents, f)
