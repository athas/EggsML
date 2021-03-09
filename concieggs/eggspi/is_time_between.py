#!/usr/bin/env python3
#
# Returner termineringskode 0 hvis nuværende tid er indeholdt i tidsinterval. Alle tider er CES?T.

import pytz
from datetime import datetime
import sys

def isNowInTimePeriod(startTime, endTime, nowTime):
    if startTime < endTime:
        return nowTime >= startTime and nowTime <= endTime
    else:
    #Over midnight:
        return nowTime >= startTime or nowTime <= endTime

tz = pytz.timezone('Europe/Copenhagen')
cph_now = datetime.now(tz)
time_zone = cph_now.strftime("%z")
cph_now = cph_now.time()
timeStart = sys.argv[1]
timeStart = datetime.strptime(timeStart + time_zone, "%H:%M%z").time()
timeEnd = sys.argv[2]
timeEnd = datetime.strptime(timeEnd + time_zone, "%H:%M%z").time()

exit(0 if isNowInTimePeriod(timeStart, timeEnd, cph_now) else 1)
