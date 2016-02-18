#!/usr/bin/env python3
#
# Group un-requests in purchase bonds, and format in a fitting JSON structure.


import subprocess
import collections
import json

time_tolerance = 60 * 60 * 24 / 2 # 12 hours
probability_include_threshold = 0.15
purchase_times_threshold = 10


def run():
    pairs = [(int(stamp), title.lower()) for stamp, title in
             (line.split(' ', 1) for line in
              subprocess.check_output(['./afønsker.py']).decode('utf-8').strip().split('\n'))]

    unrequest_times = collections.defaultdict(list)
    for time, title in pairs:
        unrequest_times[title].append(time)

    out_bonds = []
        
    for title, times in unrequest_times.items():
        if len(times) < purchase_times_threshold:
            continue
        connections_n = collections.defaultdict(int)
        for time_other, title_other in pairs:
            if title_other == title:
                continue
            for time in times:
                if time_diff(time, time_other) <= time_tolerance:
                    connections_n[title_other] += 1
                    break
        for title_other, occurences_n in connections_n.items():
            prob = occurences_n / len(times)
            if prob >= probability_include_threshold:
                out_bonds.append((title, title_other, prob))

    with open(1, 'w') as f:
        json.dump(out_bonds, f, ensure_ascii=False)

def time_diff(t0, t1):
    return abs(t0 - t1)
            
if __name__ == '__main__':
    run()
