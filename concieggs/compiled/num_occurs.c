#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  char *line = NULL;
  size_t n;
  int matches = 0;
  while (getline(&line, &n, stdin) != -1) {
    for (int i = 1; i < argc; i++) {
      char *p = line;
      while ((p = strstr(p, argv[i])) != NULL) {
        matches++;
        p += strlen(argv[i]);
      }
    }
  }
  free(line);
  printf("%d\n", matches);
}
