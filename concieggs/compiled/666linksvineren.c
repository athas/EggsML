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
      *whoend = 0;
      switch (rand()%2) {
      case 0:
        printf("%s: Det har %s allerede lænket!\n", eggsuser, whostart);
        break;
      case 1:
        printf("%s: OOOOOOOOOODLZ!!!\n", eggsuser);
        break;
      }
      /*printf("OOOOOOOOLDZ %s af %s for %ld dage siden\n", link, whostart, (now.tv_sec - then.tv_sec)/60/60/24);*/
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

  srand(now);

  if (eggsbody != NULL && eggsuser != NULL) {
    struct stat fs;

    eggsuserlength = strlen(eggsuser);
    sprintf(buf, "%s/links", getenv("CONCIEGGS_DB_DIR"));
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
