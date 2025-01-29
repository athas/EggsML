#!/usr/bin/env python3

from collections import defaultdict

kinds = [
    'andet',
    'navneord',
    'navneord_flertal',
    'flertal',
    'udsagnsord',
    'tillægsord',
    'tillægsord_flertal',
    'tillægsord_ubøjeligt',
    'forholdsord',
    'biord',
    'stedord',
    'stedord_ubøjeligt',
    'bindeord',
    'kendeord',
]

for kind, i in zip(kinds, range(len(kinds))):
    exec(f'{kind} = {i}')

def bøj(word, bøjning):
    if bøjning.startswith('-'):
        return word + bøjning[1:]
    else:
        return bøjning

word_counts = defaultdict(int)
udsagnsord_nutid_datid = []
with open('ord') as f:
    for line in f:
        word, kind, bøjninger = line.rstrip().split('¶')
        word = word.replace('|', '')
        kind = eval(kind.replace(' ', '_')) if kind else andet
        bøjninger = bøjninger.split(' ')
        bøjninger_words = [bøj(word, bøjning) for bøjning in bøjninger]
        for bøjning_word in bøjninger_words:
            word_counts[bøjning_word] += 1

        if kind == udsagnsord:
            try:
                nutid, datid, førnutid = bøjninger_words
            except ValueError:
                continue
            udsagnsord_nutid_datid.append((nutid, datid))

with open('unikke_udsagnsord_nutid_datid', 'w') as f:
    for (nutid, datid) in udsagnsord_nutid_datid:
        if word_counts[nutid] == 1:
            print(f'{nutid} {datid}', file=f)
