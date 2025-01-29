#!/usr/bin/env python3

# AT GØRE: Dette oversætter til et korrekt Kleenex-program, men
# Kleenex-oversætteren er for langsom og bruger for meget hukommelse til at
# oversætte det, fordi der er en del ord.
with open('../../compiled/datid.kex', 'w') as fw:
    print('start: message', file=fw)
    print('message := maybeSep inner maybeSep', file=fw)
    print('inner := word sep inner | word | ""', file=fw)
    print('maybeSep := /[^A-Za-zæøå0-9]*/', file=fw)
    print('sep := /[^A-Za-zæøå0-9]+/', file=fw)
    print('word ', file=fw)

    with open('unikke_udsagnsord_nutid_datid', 'r') as fr:
        sep = ':='
        for line in fr:
            nutid, datid = line.rstrip().split(' ')
            print(f'{sep} ~/{nutid}/ "{datid}"', file=fw)
            sep = '|'

    print('| /[a-zA-ZæøåÆØÅ0-9]+/', file=fw)
