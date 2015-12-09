#include <stdio.h>
#include <string.h>

int main(int argc, const char** argv) {
  int linebreak = 1;

  argv++;

  if (argc > 1 && strcmp(argv[0], "-n") == 0) {
    linebreak = 0;
    argv++;
  } else {
  }

  if (*argv) {
    fputs(*argv, stdout);
    argv++;
  }

  while (*argv) {
    fputs(" ", stdout);
    fputs(*argv, stdout);
    argv++;
  }

  if (linebreak) {
    putc('\n', stdout);
  }
}
