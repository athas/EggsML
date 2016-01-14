#!/usr/bin/env python3

import ast
import itertools
import sys

default_tolerance = 86400/2 ## 24/2 hours

def users_also_bought(waresdict):
    totals = []
    for ware, timestamps in waresdict.items():
        times_bought = len(timestamps)
        adjecent_wares = []

        for (other_ware, other_timestamps) in waresdict.items():
            if (ware == other_ware):
                continue
            times_bought_together = 0
            for timestamp in timestamps:
                for other_timestamp in other_timestamps:
                    if (timestamps_are_adjecent(timestamp, other_timestamp)):
                        times_bought_together += 1
                        break
            if (times_bought_together > 0):
                adjecent_wares.append((other_ware, times_bought_together/times_bought))
        adjecent_wares = sorted(adjecent_wares, key=lambda x: x[1], reverse=True)
        
        totals.append((ware, adjecent_wares, times_bought))

    return totals

def timestamps_are_adjecent(timestamp_x, timestamp_y, tolerance_in_seconds=default_tolerance):
    diff = abs(timestamp_x - timestamp_y)
    return (diff <= tolerance_in_seconds)

def users_also_requested():
    return

# we only import data for wares that has been afønsket granularity or more times
def import_oensker_data(filename, granularity):
    wares = {}
    with open(filename, 'r') as f:
        while True:
           next_three_lines = list(itertools.islice(f, 3))
           if not next_three_lines:
               break
           [raw_ware, raw_timestamp, whitespace] = next_three_lines
           timestamp = ast.literal_eval(raw_timestamp)
           ware = raw_ware[:-1]

           if ware in wares:
               wares[ware].append(timestamp)
           else:
               wares[ware] = []
               wares[ware].append(timestamp)
        f.close()
    kill_list = []
    for key in wares.keys():
        if (len(wares[key]) < granularity):
            kill_list.append(key)

    for key in kill_list:
        del wares[key]
    return wares


def main(filename, raw_granularity):
    granularity = int(raw_granularity)
    wares = import_oensker_data(filename, granularity)
    test  = users_also_bought(wares)

    for item in test:
        print(item)
        print()



if __name__ == '__main__':
    if (len(sys.argv) == 3):
        main(sys.argv[1], sys.argv[2])
    elif (len(sys.argv) == 2):
        main(sys.argv[1], 3)


