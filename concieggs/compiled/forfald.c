#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
  srand(getpid());
  char* line = NULL;
  size_t n;
  ssize_t m;
  while ((m = getline(&line, &n, stdin)) > 0) {
    for (int i = 0; i < m-1; i++) {
      double p = (double)i/(double)m;
      if (rand() / (double)RAND_MAX < p) {
        int j = rand() % 8;
        line[i] ^= 1<<j;
      }
    }
    fputs(line, stdout);
  }
}
