#!/usr/bin/env python3

import ast
import itertools
import sys

default_tolerance = 86400 ## 24 hours

def users_also_bought(wareslist):
    totals = []
    for (ware, timestamps) in wareslist:
        times_bought = len(timestamps)
        adjecent_wares = []

        for (other_ware, other_timestamps) in wareslist:
            times_bought_together = 0

            for timestamp in timestamps:
                for other_timestamp in other_timestamps:
                    if (timestamps_are_adjecent(timestamp, other_timestamp)):
                        times_bought_together += 1
                        break
            adjecent_wares.append((other_ware, times_bought_together/times_bought))
        totals.append((ware, adjecent_wares, times_bought))

    return totals

def timestamps_are_adjecent(timestamp_x, timestamp_y, tolerance_in_seconds=default_tolerance):
    diff = abs(timestamp_x - timestamp_y)
    return (diff <= tolerance_in_seconds)

def users_also_requested():
    return

def import_oensker_data(filename, granularity):
    wares = []
    with open(filename, 'r') as f:
        while True:
           next_three_lines = list(itertools.islice(f, 3))
           if not next_three_lines:
               break
           [ware, timestamps, whitespace] = next_three_lines
           timestamps_list = ast.literal_eval(timestamps)
           if (len(timestamps_list) >= granularity):
              wares.append((ware[:-1], timestamps_list))
        f.close()
    return wares


def main(filename, raw_granularity):
    granularity = int(raw_granularity)
    wares = import_oensker_data(filename, granularity)
    test  = users_also_bought(wares)
    print(test)
if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])

