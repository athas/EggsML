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
#include <eggc/lines.h>

int main() {
  char *input;
  char **input_lines;
  size_t nchars, nlines;
  unsigned int i, seed;

  /* First, read entire standard input into a buffer. */
  nchars = fgetall(stdin, &input);

  /* Zero-terminate it */
  input[nchars] = '\0';

  /* Then we find the lines. */
  nlines = find_lines(input, &input_lines);

  /* Set the seed. */
  FILE* f = fopen("/dev/urandom", "rb");
  fread(&seed, sizeof(unsigned int), 1, f);
  fclose(f);
  seed ^= time(NULL);
  seed ^= getpid();
  seed ^= getppid();
  srand(seed);

  /* Shuffle the lines. */
  shuffle_lines(input_lines, nlines);

  for (i = 0; i < nlines; i++) {
    puts(input_lines[i]);
  }

  return 0;
}
