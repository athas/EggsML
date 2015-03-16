#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>

int linksfd;
char* links;
int linkslength;
time_t now;
char* eggsuser;
int eggsuserlength;

char buf[1024];

void checklink(const char* link) {
  int l = strlen(link);
  char* foundlink = (char*)memmem(links, linkslength, link, l);
  int linestrlen;
  if (foundlink != NULL) {
    char* end = foundlink;
    while (!isspace(*end) && *end != 0) {
      end++;
    }
    end = 0;
    if (*(foundlink + l) == ' ') {
      char* nstart = foundlink + l + 1;
      char* whostart = strstr(nstart + 1, " ") + 1;
      char* whoend = strstr(whostart, "\n");
      time_t then = atoi(nstart);
      *whoend = 0;

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
      } else if (diff < 60 * 60 * 24 * 365) {
        unit_number = diff / 60 / 60 / 24;
        if (unit_number == 1) {
          unit = "dag";
        } else {
          unit = "dage";
        }
      } else {
        unit_number = diff / 60 / 60 / 24 / 365;
        unit = "år";
      }
      
      switch (rand()%3) {
      case 0:
        printf("%s: Det har %s allerede lænket for %d %s siden!\n",
               eggsuser, whostart, unit_number, unit);
        break;
      case 1:
        printf("%s: OOOOOOOOOODLZ!!!  Det skete jo allerede for %d %s siden!\n",
               eggsuser, unit_number, unit);
        break;
      case 2:
        printf("%s: Sikken nostalgi!  Det er jo ligesom med %s for %d %s siden!\n",
               eggsuser, whostart, unit_number, unit);
        break;
      }
    }
  }
  /* Always log */
  linestrlen = snprintf(buf, sizeof(buf), "%s %ld %s\n", link, now, eggsuser);
  write(linksfd, buf, linestrlen);
}

int main() {
  char* eggsbody;

  if (system("shuttingUp") == 0) {
    return 0;
  }

  eggsbody = getenv("EGGS_BODY");
  eggsuser = getenv("EGGS_USER");

  now = time(NULL);

  if (eggsbody != NULL && eggsuser != NULL) {
    struct stat fs;

    eggsuserlength = strlen(eggsuser);
    snprintf(buf, sizeof(buf), "%s/links", getenv("CONCIEGGS_DB_DIR"));
    linksfd = open(buf, O_RDWR | O_APPEND | O_CREAT);
    fstat(linksfd, &fs);
    linkslength = fs.st_size;
    links = mmap(NULL, linkslength, PROT_WRITE | PROT_READ, MAP_PRIVATE, linksfd, 0);
    while (1) {
      char* start, *end;
      int atend;

      start = strstr(eggsbody, "http://");

      if (start == NULL) {
        start = strstr(eggsbody, "https://");
      }

      if (start == NULL) {
        return 0;
      }

      end = start;

      while (*end != ' ' && *end != 0) {
        end++;
      }

      atend = *end == 0;
      *end = 0;

      checklink(start);

      if (atend) {
        return 0;
      }

      eggsbody = end+1;
    }
    return 0;
  } else {
    return 1;
  }
}
