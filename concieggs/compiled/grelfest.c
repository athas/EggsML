#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

int main() {
  char *eggsbody, *eggsuser;

  eggsbody = getenv("EGGS_BODY");
  eggsuser = getenv("EGGS_USER");

  time_t now = time(NULL);
  srand(now);

  if (eggsbody != NULL && eggsuser != NULL) {
    int letters = 0, uppers = 0, bad = 0;

    if (strstr(eggsbody, "!!") != NULL || strstr(eggsbody, "??") != NULL) {
      bad = 1;
    } else {
      for (letters; eggsbody[letters]; letters++) {
        uppers += isupper(eggsbody[letters]);
      }
      bad = uppers > letters/2 && uppers > 2;
    }

    if (bad) {
      return 0;
    }
  }
  
  return 1;
}
