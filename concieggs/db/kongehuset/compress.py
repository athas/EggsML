#!/usr/bin/env python3
#
# Run this program to compress contents.json into kongehuset-facts.

import collections
import json
import lzma
import struct

Content = collections.namedtuple('Content', ['title', 'paragraphs'])

with open('contents.json', 'r') as f:
    contents = json.load(f)
contents = [Content(*args) for args in contents]

text_data = b''
offsets = []
for content in contents:
    for p in content.paragraphs:
        offsets.append(len(text_data))
        text = f'{content.title.strip()}: {p}'.encode('utf-8')
        text_data += lzma.compress(text)
table_offset = len(offsets) * 4
offsets_data = b''.join(struct.pack('i', 4 + table_offset + offset) for offset in offsets)
data = struct.pack('i', len(offsets)) + offsets_data + text_data
with open('kongehuset-facts', 'wb') as f:
    f.write(data)
