#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>

int linksfd;
char* links;
int linkslength;
struct timeval now;
char* eggsuser;
int eggsuserlength;

void checklink(const char* link) {
  int l = strlen(link);
  char* foundlink = (char*)memmem(links, linkslength, link, l);
  if (foundlink != NULL) {
    char* end = foundlink;
    while (!isspace(*end) && *end != 0) {
      end++;
    }
    end = 0;
    if (*(foundlink + l) == ' ') {
      char* nstart = foundlink + l + 1;
      struct timeval then;
      then.tv_sec = atol(nstart);
      then.tv_usec = 0;
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
  // Always log
  write(linksfd, link, l);
  write(linksfd, " ", 1);
  char nowtext[24];
  sprintf(nowtext, "%ld", now.tv_sec);
  write(linksfd, nowtext, strlen(nowtext));
  write(linksfd, " ", 1);
  write(linksfd, eggsuser, eggsuserlength);
  write(linksfd, "\n", 1);
}

int main() {
  if (system("shuttingUp") == 0) {
    return 0;
  }
  char* eggsbody = getenv("EGGS_BODY");
  eggsuser = getenv("EGGS_USER");
  srand(time(NULL));
  if (eggsbody != NULL && eggsuser != NULL) {
    gettimeofday(&now, NULL);
    eggsuserlength = strlen(eggsuser);
    char linkspath[1024];
    sprintf(linkspath, "%s/links", getenv("CONCIEGGS_DB_DIR"));
    linksfd = open(linkspath, O_RDWR | O_APPEND | O_CREAT);
    struct stat fs;
    fstat(linksfd, &fs);
    linkslength = fs.st_size;
    links = mmap(NULL, linkslength, PROT_WRITE | PROT_READ, MAP_PRIVATE, linksfd, 0);
    while (1) {
      char* start = strstr(eggsbody, "http://");
      if (start == NULL) {
        start = strstr(eggsbody, "https://");
      }
      if (start == NULL) {
        return 0;
      }
      char* end = start;
      while (*end != ' ' && *end != 0) {
        end++;
      }
      int atend = *end == 0;
      *end = 0;
      checklink(start);
      if (atend) {
        return 0;
      }
      eggsbody = end+1;
    }
  }
}
