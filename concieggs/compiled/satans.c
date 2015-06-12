#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>

static unsigned int satans_chance = 5; /* In percent. */

int main() {
  int c;
  int inword = 0;
  unsigned int seed;

  seed ^= time(NULL);
  seed ^= getpid();
  seed ^= getppid();
  srand(seed);

  while ((c = getchar()) != EOF) {
    if (inword && !isalpha(c) && rand() % 100 < satans_chance) {
      puts("...");
      fflush(stdout);
      sleep(1);
      puts("...");
      fflush(stdout);
      sleep(2);
      puts("Satans!");
      return 0;
    }
    if (isalpha(c)) {
      inword = 1;
    } else {
      inword = 0;
    }
    putchar(c);
  }
}
