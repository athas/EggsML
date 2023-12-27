#!/usr/bin/env python3
#
# Get a fact about Kongehuset.

import sys
import os.path
import lzma
import struct
import random

path = os.path.join(os.path.dirname(__file__), 'kongehuset-facts')
try:
    with open(path, 'rb') as f:
        data = f.read()
except FileNotFoundError:
    sys.exit(1)

n_entries = struct.unpack('i', data[:4])[0]
entry_i = random.randrange(n_entries)
i = 4 + 4 * entry_i
offset_start, offset_end = struct.unpack('ii', data[i:i + 8])
if entry_i == n_entries - 1:
    offset_end = len(data)
d = lzma.decompress(data[offset_start:offset_end])
with open(1, 'wb') as f:
    f.write(d)
    f.write(b'\n')
