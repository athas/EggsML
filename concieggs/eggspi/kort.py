#!/usr/bin/env python3

from functools import reduce
from random import shuffle
import re
import subprocess
import sys
import eggsthon.sted as sted

USE_RANDOM_COLOURS = True

colour_mapping = {
        b'69': b'12',  # blue
        b'71': b'03',  # green
        b'103': b'13', # purple
        b'107': b'09', # yellow-green
        b'131': b'05', # purple-brown
        b'146': b'11', # grey-blue
        b'181': b'06', # pale red brown
        b'196': b'04', # red
        b'222': b'08', # yellowish
        b'254': b'14', # grey
        b'255': b'15', # light grey
}

removes = [b'\x1b[2J', b'\x1b[?6h', b'\r', b'\x1b\[39;48;5;16m', b';48;5;16',
        b'\x1b[39m', b'\x1b[39;49m'
        ]
p_fg = re.compile(b'\x1b\[38;5;(\d+)m')
p_coords = re.compile('^-?\d+(.\d*)?$')
p_size = re.compile('\((\d+)x(\d+)\)')

unused_colours = list(range(2, 16))
shuffle(unused_colours)
random_colour_mapping = {}

def get_colour(ansi_colour):
    if USE_RANDOM_COLOURS and ansi_colour not in random_colour_mapping:
        random_colour_mapping[ansi_colour] = bytes(
            f'{unused_colours.pop() if unused_colours else 0:02d}', 'ascii')
    elif not USE_RANDOM_COLOURS:
        return colour_mapping.get(ansi_colour, b'00')
    return random_colour_mapping[ansi_colour]

def sub_fg(match):
    fg = match.group(1)
    return b'\x03' + get_colour(fg)

def make_map(lat, lon, zoom, width, height):
    output = subprocess.check_output(
            f'~/node_modules/.bin/mapscii -H -w {width} -h {height} -z {zoom} --lat {lat} --lon {lon}',
            shell=True)

    for _remove in removes:
        output = output.replace(_remove, b'')

    output = p_fg.sub(sub_fg, output)
    output.replace(b'\x1b\[39;49m', b'\x03')
    return output.decode('utf8')

if __name__ == '__main__':
    locationFound = False
    width, height = 40, 10
    if len(sys.argv) > 1:
        # let us search for that name
        query = " ".join(sys.argv[1:])
        if "fra en DC9" in query:
            query = query.replace("fra en DC9", "")
        if "fra en rumstation" in query:
            query = query.replace("fra en rumstation", "")
        (lat, lon) = sted.get_coords(query)
        lat = "{:2.6f}".format(lat)
        lon = "{:2.6f}".format(lon)
        locationFound = p_coords.match(lat) and p_coords.match(lon)
        m = p_size.search(query)
        if m:
            width, height = m.groups()
        if int(height) > 40:
            print('Max højde er 40')
            sys.exit(1)
        if int(width) > 85:
            print('Max bredde er 85')
            sys.exit(1)
    if not locationFound and len(sys.argv) >= 3:
        lat, lon = sys.argv[1:3]
        locationFound = p_coords.match(lat) and p_coords.match(lon)
    if locationFound:
        zoom = 15
        if "fra en DC9" in " ".join(sys.argv[1:]):
            zoom = 9
        if "fra en rumstation" in " ".join(sys.argv[1:]):
            zoom = 4
        output = make_map(lat, lon, zoom, width, height)
        sys.stdout.write(output)
        sys.exit(0)

    print('Brug: kort LÆ.NGDEGRAD BR.EDDEGRAD')
    sys.exit(1)
