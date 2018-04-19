#!/usr/bin/env python3

import os.path
import glob
import re


os.chdir(os.path.dirname(__file__))

paragraphs = []

with open('taler-markov-ready', 'w') as fw:
    for fname in sorted(glob.glob('????.txt')):
        with open(fname) as fr:
            paragraphs.extend('\n\n'.join(re.sub(r'([,;:])', r'\n\1',
                                                 sentence.replace(' ', '\n')) + '\n.'
                                          for sentence in re.split(r'(?<!\d)[.!] ', paragraph.rstrip('.!')))
                              for paragraph in fr.read().split('\n'))
    fw.write('\n\n\n'.join(paragraphs))
