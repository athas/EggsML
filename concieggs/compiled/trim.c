// Remove leading and trailing whitespace from stdin.

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

char* strtrim(char *s) {
  while (*s && isspace(*s)) { s++; }
  char *first_nonws = s;
  char *last_nonws = s;
  while (*s) {
    if (!isspace(*s)) {
      last_nonws = s;
    }
    s++;
  }
  if (*last_nonws) {
    *(last_nonws+1) = 0;
  }
  return first_nonws;
}

int main() {
  char *line = NULL;
  size_t buf;
  ssize_t n;

  while ((n = getline(&line, &buf, stdin)) != -1) {
    fputs(strtrim(line), stdout);
  }
  free(line);
}
