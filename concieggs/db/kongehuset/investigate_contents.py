#!/usr/bin/env python3
#
# Run this program to print contents.json in a more readable way in order to
# investigate it.

import collections
import json
import textwrap

Content = collections.namedtuple('Content', ['title', 'paragraphs'])

with open('contents.json', 'r') as f:
    contents = json.load(f)
contents = [Content(*args) for args in contents]

print('-' * 70)
print('')
print(f'Antal: {len(contents)}')
print('')
print('-' * 70)
for content in contents:
    print('')
    print(content.title)
    print('')
    for p in content.paragraphs:
        print(textwrap.fill(p, initial_indent='  ', subsequent_indent='  '))
        print('')
    print('-' * 70)
