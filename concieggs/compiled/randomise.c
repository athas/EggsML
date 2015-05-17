/*
 * Read lines from standard input, and print a random permutation on
 * standard output.  Will keep all lines in memory, so do not use it
 * with grossly large input.
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>

char *argv0;
char *input;
size_t input_buffer_size, input_read;

/* Read all of a file into a global buffer, because I am so lazy. */
void fgetall(FILE* file) {
  for (;;) {
    if (input_buffer_size - input_read == 0) {
      input_buffer_size *= 2;
      input = realloc(input, input_buffer_size + 1);
    }
    errno = 0;
    input_read += fread(input, sizeof(char), input_buffer_size - input_read, file);
    if (feof(file)) {
      return;
    }
    if (errno != 0) {
        perror(argv0);
    }
  }
}

/* Find line boundaries and replace them with zero-bytes. */
void find_lines(char *input, char **lines[], size_t *nlines) {
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
  *nlines = lines_seen;
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

int main() {
  unsigned int i, seed = 0;
  char **lines;
  size_t nlines;

  input_buffer_size = 8 * 1024;
  input_read = 0;
  input = malloc(input_buffer_size + 1);

  /* First, read entire standard input into a buffer. */
  fgetall(stdin);

  /* Zero-terminate it */
  input[input_read] = '\0';

  /* Then we find the lines. */
  find_lines(input, &lines, &nlines);

  /* Shuffle the lines. */
  seed ^= time(NULL);
  seed ^= getpid();
  seed ^= getppid();
  srand(seed);
  shuffle_lines(lines, nlines);

  for (i = 0; i < nlines; i++) {
    puts(lines[i]);
  }

  return 0;
}
