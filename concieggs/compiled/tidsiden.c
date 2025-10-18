#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <stdlib.h>

int main(int argc, const char** argv) {
  if (argc != 2) {
    fprintf(stderr, "Nej, sådan her: %s SEKUNDER-SIDEN-1970\n", argv[0]);
    return 1;
  }

  time_t now = time(NULL);
  time_t then = atoi(argv[1]);
  time_t diff = now - then;

  char* unit;
  int unit_number;

  if (diff < 60) {
    if (diff == 1) {
      unit = "sekund";
    } else {
      unit = "sekunder";
    }
    unit_number = diff;
  } else if (diff < 60 * 60) {
    unit_number = diff / 60;
    if (unit_number == 1) {
      unit = "minut";
    } else {
      unit = "minutter";
    }
  } else if (diff < 60 * 60 * 24) {
    unit_number = diff / 60 / 60;
    if (unit_number == 1) {
      unit = "time";
    } else {
      unit = "timer";
    }
  } else if (diff < 60 * 60 * 24 * 12) {
    unit_number = diff / 60 / 60 / 24;
    if (unit_number == 1) {
      unit = "dag";
    } else {
      unit = "dage";
    }
  } else if (diff < 60 * 60 * 24 * 365) {
    unit_number = diff / 60 / 60 / 24 / 30;
    if (unit_number == 1) {
      unit = "måned";
    } else {
      unit = "måneder";
    }
  } else {
    unit_number = diff / 60 / 60 / 24 / 365;
    unit = "år";
  }

  printf("%d %s", unit_number, unit);

}
