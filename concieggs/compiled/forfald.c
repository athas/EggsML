#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <math.h>

int main(int argc, char* argv[]) {
  srand(getpid());
  char* line = NULL;
  size_t n;
  ssize_t m;
  while ((m = getline(&line, &n, stdin)) > 0) {
    for (int i = 0; i < m-2; i++) {
      double p = exp(0.3*(double)i/(double)m)-1;
      if (rand() / (double)RAND_MAX < p) {
        int j = rand() % 7;
        line[i] ^= 1<<j;
      }
    }
    fputs(line, stdout);
  }
}
