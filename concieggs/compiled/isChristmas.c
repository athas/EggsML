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

double christmasLevel(time_t rawtoday)
{
    struct tm begin, end, *today;
    time_t rawbegin, rawend;
    double days, maxdays, level;

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

    level = pow(1.1, days) / pow(1.1, maxdays);

    if (today->tm_wday == 0) {
        struct tm in_four_weeks, end_of_advent;
        time_t raw_in_four_weeks, raw_end_of_advent;

        in_four_weeks = *today;
        in_four_weeks.tm_mday += 4*7;

        end_of_advent = (struct tm) { 0, 0, 0, 25, DECEMBER, today->tm_year, 0, 0, 0 };

        raw_in_four_weeks = mktime(&in_four_weeks);
        raw_end_of_advent = mktime(&end_of_advent);

        if (difftime(raw_end_of_advent, rawtoday) > 0 || difftime(raw_in_four_weeks, raw_end_of_advent) > 0) {
            level += 0.2;
        }
    }

    if (level > 1) level = 1;

    return 100*level;
}

bool isChristmas(time_t rawtoday)
{
    return christmasLevel(rawtoday) > 0;
}

int main(int argc, char *argv[])
{
    time_t rawtoday = time(NULL);

    if (strstr(argv[0], "christmasLevel") != NULL) {
        printf("%.2f", christmasLevel(rawtoday));
        return 0;
    } else if (strstr(argv[0], "isChristmas") != NULL) {
        return !isChristmas(rawtoday); // 0 is success
    }

    return 0;
}
