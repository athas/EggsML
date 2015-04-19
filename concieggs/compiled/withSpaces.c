#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

ssize_t getline1(char** line) {
  size_t len = 0;
  ssize_t read;
  
  read = getline(line, &len, stdin);
  if (read == -1) {
    return read;
  }
  if ((*line)[read - 1] == '\n') {
    (*line)[read - 1] = '\0';
  }
  return read;
}

int main() {
  char *line = NULL;
  ssize_t read;

  read = getline1(&line);
  if (read != -1) {
    printf("%s", line);
  }
  while ((read = getline1(&line)) != -1) {
    printf(" %s", line);
  }
  return EXIT_SUCCESS;
}
