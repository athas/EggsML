// Genererer et juleniveau mellem 0 og 150.  Desto tættere på 100, desto mere
// julet. Virker kun mellem 24. november og 25. december, begge dage inklusiv.

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>
#include <string.h>

// struct tm's .tm_mon ligger i intervallet [0, 11].
#define NOVEMBER (10)
#define DECEMBER (11)

/* x_time is within the time interval [begin_time, end_time]. */
#define WITHIN_TIME_INTERVAL(begin_time, x_time, end_time) \
  (difftime((end_time), (x_time)) >= 0 && \
   difftime((x_time), (begin_time)) >= 0)

double christmasLevel(time_t raw_today)
{
    struct tm *today, begin, end, eve_begin, eve_end;
    time_t raw_begin, raw_end, raw_eve_begin, raw_eve_end;
    double days, max_days, level;

    today = localtime(&raw_today);
    begin = (struct tm) { 0, 0, 0, 24, NOVEMBER, today->tm_year, 0, 0, 0 };
    end = (struct tm) { 0, 0, 0, 26, DECEMBER, today->tm_year, 0, 0, 0 };

    raw_begin = mktime(&begin);
    raw_end = mktime(&end);

    if (!WITHIN_TIME_INTERVAL(raw_begin, raw_today, raw_end)) {
        return 0;
    }

    // Let Christmas eve be [YYYY-12-24 00:00:00 ; YYYY-12-25 06:00:00.
    eve_begin = (struct tm) { 0, 0, 0, 24, DECEMBER, today->tm_year, 0, 0, 0 };
    eve_end = (struct tm) { 0, 0, 6, 25, DECEMBER, today->tm_year, 0, 0, 0 };

    raw_eve_begin = mktime(&eve_begin);
    raw_eve_end = mktime(&eve_end);

    // Max out Christmas level on Christmas eve.
    if (WITHIN_TIME_INTERVAL(raw_eve_begin, raw_today, raw_eve_end)) {
        return 150;
    }


    days = difftime(raw_today, raw_begin) / (60*60*24);
    max_days = difftime(raw_end, raw_begin) / (60*60*24);

    level = pow(1.1, days) / pow(1.1, max_days);

    // Boost christmas level on Advent Sundays!
    if (today->tm_wday == 0) {
        struct tm in_four_weeks, end_of_advent;
        time_t raw_in_four_weeks, raw_end_of_advent;

        in_four_weeks = *today;
        in_four_weeks.tm_mday += 4*7;

        end_of_advent = (struct tm) { 0, 0, 0, 25, DECEMBER, today->tm_year, 0, 0, 0 };

        raw_in_four_weeks = mktime(&in_four_weeks);
        raw_end_of_advent = mktime(&end_of_advent);

        if (difftime(raw_end_of_advent, raw_today) > 0 || difftime(raw_in_four_weeks, raw_end_of_advent) > 0) {
            level = level > 0.8 ? level : 0.8;
        }
    }

    return 100*level;
}

char *christmasLevelString(time_t raw_today)
{
    double level = christmasLevel(raw_today);
    if (level >= 90) return "satans";
    if (level >= 80) return "fandme";
    if (level >= 70) return "seriøst";
    if (level >= 60) return "virkelig";
    if (level >= 50) return "da";
    if (level >= 40) return "nogenlunde";
    if (level >= 30) return "en anelse";
    return "ikke særlig";
}

bool isChristmas(time_t raw_today)
{
    return christmasLevel(raw_today) > 0;
}

int main(int argc, char *argv[])
{
    time_t raw_today = time(NULL);

    if (strstr(argv[0], "christmasLevel") != NULL) {
        if (argc == 2 && strstr(argv[1], "-s") != NULL) {
            printf("%s", christmasLevelString(raw_today));
        } else {
            printf("%f", christmasLevel(raw_today));
        }

        return 0;
    } else if (strstr(argv[0], "isChristmas") != NULL) {
        return !isChristmas(raw_today); // 0 is success
    }

    return 0;
}
