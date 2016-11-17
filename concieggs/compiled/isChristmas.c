#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>
#include <string.h>

// Genererer et juleniveau mellem 0 og 100.  Desto tættere på 100, desto mere
// julet. Virker kun mellem 24. november og 25. december, begge dage inklusiv.

// struct tm's .tm_mon ligger i intervallet [0, 11].
#define NOVEMBER (10)
#define DECEMBER (11)

int christmasLevel(time_t rawtoday)
{
    struct tm begin, end, *today;
    time_t rawbegin, rawend;
    double days, maxdays;

    today = localtime(&rawtoday);
    begin = (struct tm) { 0, 0, 0, 24, NOVEMBER, today->tm_year, 0, 0, 0 };
    end = (struct tm) { 0, 0, 0, 26, DECEMBER, today->tm_year, 0, 0, 0 };

    rawbegin = mktime(&begin);
    rawend = mktime(&end);

    if (difftime(rawend, rawtoday) < 0 || difftime(rawtoday, rawbegin) < 0) {
        return 0;
    }

    days = difftime(rawtoday, rawbegin) / (60*60*24);
    maxdays = difftime(rawend, rawbegin) / (60*60*24);

    return (int)(pow(1.1, days) / pow(1.1, maxdays) * 100);
}

bool isChristmas(time_t rawtime)
{
    return christmasLevel(rawtime) > 0;
}

int main(int argc, char *argv[])
{
    time_t rawtime = time(NULL);

    if (strstr(argv[0], "christmasLevel") != NULL) {
        return christmasLevel(rawtime);
    } else if (strstr(argv[0], "isChristmas") != NULL) {
        return !isChristmas(rawtime); // 0 is success
    }

    return 0;
}
