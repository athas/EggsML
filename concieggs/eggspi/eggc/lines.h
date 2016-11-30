#ifndef __EGGC_LINES_H__
#define __EGGC_LINES_H__

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

/* Read all of file into *buf. */
size_t fgetall(FILE *file, char **buf) {
  size_t bufsize = 8 * 1024;
  size_t bufread = 0;

  *buf = malloc(bufsize + 1);

  for (;;) {
    if (bufsize - bufread == 0) {
      bufsize *= 2;
      *buf = realloc(*buf, bufsize + 1);
    }
    errno = 0;
    bufread += fread(*buf, sizeof(char), bufsize - bufread, file);
    if (feof(file)) {
      return bufread;
    }
    if (errno != 0) {
      perror(NULL);
    }
  }
}

/* Find line boundaries and replace them with zero-bytes. */
size_t find_lines(char *input, char **lines[]) {
  size_t lines_buffer_size = 8 * 1024;
  size_t lines_seen = 0;
  *lines = malloc(lines_buffer_size * sizeof(char*));

  /* First line always starts at first character. */
  (*lines)[lines_seen++] = input;

  for (; *input; input++) {
    if (*input == '\n') {
      if (lines_seen == lines_buffer_size) {
        lines_buffer_size *= 2;
        *lines = realloc(*lines, lines_buffer_size * sizeof(char*));
      }
      *input = '\0';
      /* Final line may contain a terminating newline if it wants
         to. */
      if (!*(input+1)) {
        break;
      }
      (*lines)[lines_seen++] = input + 1;
    }
  }
  return lines_seen;
}

/* Fisher-Yates shuffle the lines. */
void shuffle_lines(char *lines[], size_t nlines) {
  unsigned int i;
  for (i = nlines - 1; i >= 1; i--) {
    unsigned int j = rand() % (i + 1);
    char *line_j = lines[j];
    lines[j] = lines[i];
    lines[i] = line_j;
  }
}

#endif /* __EGGC_LINES_H__ */
