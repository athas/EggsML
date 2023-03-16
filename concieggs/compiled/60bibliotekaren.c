#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

const char* svinere[] =
  { "/me stirrer indgående på %s.",
    "%s: Vil du være sød at dæmpe dig lidt?",
    "%s: Ssh!",
    "/me kigger på %s og tysser.",
    "%s: Vær sød ikke at råbe.",
    "%s: Her holder vi altså et civilt toneleje.",
    "%s: Skal du hjælpes over på en mere larmende kanal?",
    "%s: Dæmp dig.",
    "/me kigger på %s og rømmer sig højlydt.",
    "/me rømmer sig."
  };

int main() {
  char *eggsbody, *eggsuser;

  if (system("shuttingUp") == 0) {
    return 0;
  }

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
      bad = uppers > letters/2 && letters > 2;
    }

    if (bad) {
      printf(svinere[rand()%(sizeof(svinere)/sizeof(svinere[0]))], eggsuser);
      putchar('\n');
    }
  }
}
